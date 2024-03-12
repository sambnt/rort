{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Rort.Render where

import Rort.Vulkan.Context ( VkContext, VkContext(..), vkGraphicsQueue, vkTransferQueue, vkTransferQueueIx, vkGraphicsQueueIx, vkPresentationQueue )
import Rort.Render.Swapchain (Swapchain, vkImageViews, vkExtent, throwSwapchainOutOfDate, throwSwapchainSubOptimal, vkSwapchain, SwapchainOutOfDate (SwapchainOutOfDate), vkDepthFormat, vkSwapchainHeight, vkSwapchainWidth)
import Rort.Render.Types ( handleGet, Subpass(..), DrawRenderPass(..), Attachment(..), DrawCall(PrimitiveDraw, IndexedDraw), Subpass(Subpass), DrawCallPrimitive(..), SubpassInfo(..), Draw(..), DrawCallIndexed(..), Shader(Shader), pipelineShaderStage, Handle (ShaderHandle, RenderPassHandle, TextureHandle), ShaderInfo (..), Buffer(Buffer), BufferInfo (..), RenderPass (RenderPass), unsafeGetHandle, TextureInfo (TextureInfo), Texture(Texture), DrawDescriptor (..), AttachmentFormat (..), toAttachmentDescription, DrawRenderPass, DrawSubpass (DrawSubpass), RenderPassInfo(..), BufferCopy (..), BufferImageCopyInfo (..), BufferCopyInfo(..), NewHandle, mkHandle, Load)
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.Vector as Vector
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import Rort.Vulkan (withVkRenderPass, withVkGraphicsPipelines, withVkFramebuffer, withVkPipelineLayout, withVkShaderModule, withVkCommandBuffers, withSemaphore, withVkDescriptorSetLayout, withSampler, withImageView)
import qualified Vulkan.CStruct.Extends as Vk
import Data.Bits ((.|.), (.&.))
import Control.Monad (forM, forM_, unless)
import Data.Functor ((<&>), void)
import Rort.Render.FramesInFlight (FrameSync (..), FramesInFlight, NumFramesInFlight, withFramesInFlight, FrameInFlight (FrameInFlight), withNextFrameInFlight)
import Data.Word (Word32, Word64)
import Control.Monad.IO.Class (MonadIO (..))
import Foreign (nullPtr, castPtr, pokeArray, Storable)
import Data.Acquire (Acquire, allocateAcquire, with)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Rort.Util.Defer (defer, evalEmpty, eval, unsafeGet)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO, runResourceT, release)
import Control.Exception.Safe (MonadMask, try, finally, mask, onException)
import Rort.Allocator (withBuffer, withAllocPtr, flush, getAllocData, getAllocOffset, requiresBufferCopy, withImage, Allocator, withImageAttachment)
import Control.Concurrent.STM (TQueue, newTQueueIO, writeTQueue)
import qualified Control.Concurrent.STM as STM
import Rort.Render.SwapchainImages (SwapchainImages, SwapchainImage (SwapchainImage))
import qualified Rort.Render.SwapchainImages as Swapchain
import Data.Vector (Vector)
import Data.Function ((&))
import Control.Monad.State (runStateT)
import Control.Monad.Reader (runReaderT)

data Renderer
  = Renderer { rendererSwapchain      :: SwapchainImages
             , rendererFramesInFlight :: FramesInFlight
             , rendererPasses         :: TQueue (Handle RenderPass)
             , rendererDevice         :: Vk.Device
             , rendererPendingWrites  :: TQueue BufferCopy
             }

recordCopies
  :: MonadResource m
  => VkContext
  -> Vk.CommandPool
  -> Vk.CommandBuffer
  -> [BufferCopy]
  -> m ()
recordCopies ctx cmdPool cmdBuffer xs = do
  let
    (bufferCopies, imgCopies) =
        foldl (\(accBuf, accImg) ->
                 \case
                   CopyToBuffer bufCopy -> (bufCopy:accBuf, accImg)
                   CopyToImage imgCopy -> (accBuf, imgCopy:accImg)
              ) mempty xs

  recordBufferCopies ctx cmdPool cmdBuffer bufferCopies
  recordBufferImageCopies ctx cmdPool cmdBuffer imgCopies

recordBufferImageCopies
  :: MonadResource m
  => VkContext
  -> Vk.CommandPool
  -> Vk.CommandBuffer
  -> [BufferImageCopyInfo]
  -> m ()
recordBufferImageCopies _ctx _cmdPool _cmdBuffer [] = pure ()
recordBufferImageCopies ctx _cmdPool cmdBuffer bufferImageCopyInfos = do
  let
    memBarriers = flip foldMap bufferImageCopyInfos $ \copy ->
      let
        imgRange =
          Vk.ImageSubresourceRange
            Vk.IMAGE_ASPECT_COLOR_BIT
            0 -- baseMipLevel
            1 -- levelCount
            0 -- baseArrayLayer
            1 -- layerCount

        preCopyMemoryBarrier =
          Vk.ImageMemoryBarrier2
            ()
            Vk.zero -- src stage mask
            Vk.zero -- src access mask
            Vk.PIPELINE_STAGE_2_TRANSFER_BIT_KHR -- dst stage mask
            Vk.ACCESS_2_MEMORY_WRITE_BIT -- dst access mask
            Vk.IMAGE_LAYOUT_UNDEFINED -- old layout
            Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL -- new layout
            Vk.QUEUE_FAMILY_IGNORED -- srcQueueFamilyIx
            Vk.QUEUE_FAMILY_IGNORED -- dstQueueFamilyIx
            copy.dstImage -- image
            imgRange -- subresource range
      in
        [preCopyMemoryBarrier]

  let
    dependencyInfo = Vk.DependencyInfo
      Vk.zero -- dependency flags
      Vector.empty -- memory barriers
      Vector.empty -- buffer memory barriers
      (Vector.fromList $ Vk.SomeStruct <$> memBarriers) -- image memory barriers

  Vk.cmdPipelineBarrier2KHR cmdBuffer dependencyInfo

  forM_ bufferImageCopyInfos $ \copy -> do
    Vk.cmdCopyBufferToImage
      cmdBuffer
      copy.srcBuffer
      copy.dstImage
      Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      copy.regions

  if vkGraphicsQueue ctx == vkTransferQueue ctx
  then do
    let
      imgRange =
        Vk.ImageSubresourceRange
          Vk.IMAGE_ASPECT_COLOR_BIT
          0 -- baseMipLevel
          1 -- levelCount
          0 -- baseArrayLayer
          1 -- layerCount

      postCopyMemoryBarriers = flip foldMap bufferImageCopyInfos $ \copy ->
        [ Vk.ImageMemoryBarrier2
            ()
            Vk.PIPELINE_STAGE_2_TRANSFER_BIT -- src stage mask
            Vk.ACCESS_2_TRANSFER_WRITE_BIT_KHR -- src access mask
            Vk.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR -- dst stage mask
            Vk.ACCESS_2_SHADER_READ_BIT -- dst access mask
            Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL -- old layout
            Vk.IMAGE_LAYOUT_READ_ONLY_OPTIMAL -- new layout
            Vk.QUEUE_FAMILY_IGNORED -- srcQueueFamilyIx
            Vk.QUEUE_FAMILY_IGNORED -- dstQueueFamilyIx
            copy.dstImage -- image
            imgRange -- subresource range
        ]

      postCopyDependencyInfo = Vk.DependencyInfo
        Vk.zero -- dependency flags
        Vector.empty -- memory barriers
        Vector.empty -- buffer memory barriers
        (Vector.fromList $ Vk.SomeStruct <$> postCopyMemoryBarriers) -- image memory barriers

    Vk.cmdPipelineBarrier2KHR cmdBuffer postCopyDependencyInfo
  else error "Separate transfer and graphics queue for buffer image copy not implemented!"

recordBufferCopies
  :: MonadResource m
  => VkContext
  -> Vk.CommandPool
  -> Vk.CommandBuffer
  -> [BufferCopyInfo]
  -> m ()
recordBufferCopies _ctx _cmdPool _cmdBuffer [] = pure ()
recordBufferCopies ctx cmdPool cmdBuffer bufferCopyInfos = do
  forM_  bufferCopyInfos $ \copy ->
    Vk.cmdCopyBuffer cmdBuffer copy.srcBuffer copy.dstBuffer copy.regions

  if vkGraphicsQueue ctx == vkTransferQueue ctx
  then do
    let
      barriers = flip foldMap bufferCopyInfos $ \copy ->
        [ Vk.MemoryBarrier2
            Vk.PIPELINE_STAGE_2_TRANSFER_BIT_KHR -- src stage mask
            Vk.ACCESS_2_MEMORY_WRITE_BIT_KHR -- src access mask
            copy.dstStageMask -- dst stage mask
            Vk.ACCESS_2_MEMORY_READ_BIT_KHR -- dst access mask
        ]

    let
      dependencyInfo = Vk.DependencyInfo
        Vk.zero -- dependency flags
        (Vector.fromList barriers) -- memory barriers
        Vector.empty -- buffer memory barriers
        Vector.empty -- image memory barriers

    Vk.cmdPipelineBarrier2KHR cmdBuffer dependencyInfo
  else do
    _ <- error "Separate transfer and graphics queue not implemented"
    let
      gfxWaitSemaphoreInfo = Vk.SemaphoreCreateInfo () Vk.zero

    (_, gfxWaitSemaphore) <- allocateAcquire $
      withSemaphore (vkDevice ctx) gfxWaitSemaphoreInfo

    (_, cmdBuffers) <-
      allocateAcquire $ withVkCommandBuffers
        (vkDevice ctx)
        $ Vk.CommandBufferAllocateInfo
            cmdPool
            -- Primary = can be submitted to queue for execution, can't be
            -- called by other command buffers.
            Vk.COMMAND_BUFFER_LEVEL_PRIMARY
            1 -- count
    let transferCmdBuffer = Vector.head cmdBuffers

    Vk.beginCommandBuffer transferCmdBuffer
      $ Vk.CommandBufferBeginInfo
          ()
          Vk.zero
          Nothing -- Inheritance info

    let
      barriers = flip foldMap bufferCopyInfos $ \copy ->
        flip foldMap copy.regions $ \region ->
          [ Vk.BufferMemoryBarrier2
              Vk.PIPELINE_STAGE_2_TRANSFER_BIT_KHR -- src stage mask
              Vk.ACCESS_2_MEMORY_WRITE_BIT_KHR -- src access mask
              Vk.zero -- dst stage mask
              Vk.zero -- dst access mask
              (vkTransferQueueIx ctx) -- src queue family ix
              (vkGraphicsQueueIx ctx) -- dst queue family ix
              copy.dstBuffer -- buffer
              region.dstOffset -- offset
              region.size -- size
          ]

    let
      dependencyInfo = Vk.DependencyInfo
        Vk.zero -- dependency flags
        Vector.empty -- memory barriers
        (Vector.fromList barriers) -- buffer memory barriers
        Vector.empty -- image memory barriers

    Vk.cmdPipelineBarrier2KHR transferCmdBuffer dependencyInfo

    Vk.endCommandBuffer transferCmdBuffer

    let
      submitInfo = Vk.SubmitInfo
        ()
        mempty -- wait semaphores
        mempty -- wait dst stage mask
        (Vector.singleton $ Vk.commandBufferHandle transferCmdBuffer) -- commandBuffers
        (Vector.singleton gfxWaitSemaphore) -- signal semaphores

    Vk.queueSubmit (vkTransferQueue ctx) (Vector.singleton $ Vk.SomeStruct submitInfo) Vk.NULL_HANDLE

    let
      barriers2 = flip foldMap bufferCopyInfos $ \copy ->
        flip foldMap copy.regions $ \region ->
          [ Vk.BufferMemoryBarrier2
              Vk.zero -- src stage mask
              Vk.zero -- src access mask
              copy.dstStageMask -- dst stage mask
              Vk.ACCESS_2_MEMORY_READ_BIT -- dst access mask
              (vkTransferQueueIx ctx) -- src queue family ix
              (vkGraphicsQueueIx ctx) -- dst queue family ix
              copy.dstBuffer -- buffer
              region.dstOffset -- offset
              region.size -- size
          ]

    let
      dependencyInfo2 = Vk.DependencyInfo
        Vk.zero -- dependency flags
        Vector.empty -- memory barriers
        (Vector.fromList barriers2) -- buffer memory barriers
        Vector.empty -- image memory barriers

    Vk.cmdPipelineBarrier2KHR cmdBuffer dependencyInfo2
    -- pure $ Just (gfxWaitSemaphore, foldl (\acc copy -> acc .|. copy.dstStageMask) Vk.zero bufferCopyInfos)

createRenderer
  :: MonadResource m
  => VkContext
  -> NumFramesInFlight
  -> m Renderer
createRenderer ctx numFramesInFlight = do
  swapchainImgs <- Swapchain.create ctx

  framesInFlight <-
    withFramesInFlight (vkDevice ctx) (vkQueueFamilies ctx) numFramesInFlight

  renderPassQue <- liftIO newTQueueIO

  pendingWrites <- liftIO newTQueueIO

  pure $
    Renderer
      swapchainImgs
      framesInFlight
      renderPassQue
      (vkDevice ctx)
      pendingWrites

submit :: (MonadResource m, MonadMask m, MonadUnliftIO m) => VkContext -> Renderer -> (Swapchain -> m [DrawRenderPass]) -> m ()
submit ctx r getDraws = do
  result <- try $ Swapchain.withSwapchainImage r.rendererSwapchain $ \(SwapchainImage _ swapchain) -> do
    rpDraws <- getDraws swapchain
    -- Eval all
    forM_ rpDraws $ \rpDraw -> do
      _ <- evalRenderPass (vkAllocator ctx) (vkDevice ctx) swapchain rpDraw.drawRenderPass
      forM_ rpDraw.drawSubpasses $ \(DrawSubpass draws) -> do
        forM_ draws $ \draw -> do
          let buffers = drawVertexBuffers draw <> fmap fst (drawIndexBuffers draw)
          mapM_ (evalBuffer ctx r) buffers

          let
            textures = flip foldMap (concat $ drawDescriptors draw) $ \case
              (DescriptorUniform _) -> []
              (DescriptorTexture t) -> [t]
          mapM_ (evalTexture ctx r) textures

    withNextFrameInFlight
      (vkDevice ctx)
      (rendererFramesInFlight r)
      $ \(FrameInFlight fs descPool cmdPool) -> runResourceT $ do
        (_, cmdBuffers) <-
          allocateAcquire $ withVkCommandBuffers
            (vkDevice ctx)
            $ Vk.CommandBufferAllocateInfo
                cmdPool
                -- Primary = can be submitted to queue for execution, can't be
                -- called by other command buffers.
                Vk.COMMAND_BUFFER_LEVEL_PRIMARY
                1 -- count
        let cmdBuffer = Vector.head cmdBuffers
        finallyPresent (vkDevice ctx) (vkGraphicsQueue ctx) (vkPresentationQueue ctx) (vkSwapchain swapchain) fs $ \imageIndex -> do
          Vk.beginCommandBuffer cmdBuffer
            $ Vk.CommandBufferBeginInfo
                ()
                Vk.zero
                Nothing -- Inheritance info
          mask $ \restore -> do
            bufferCopies <- liftIO $ STM.atomically $ STM.flushTQueue r.rendererPendingWrites
            unless (null bufferCopies) $ liftIO $ print bufferCopies
            restore (recordCopies ctx cmdPool cmdBuffer bufferCopies)
              `onException` liftIO (STM.atomically $ forM_ bufferCopies $ STM.writeTQueue r.rendererPendingWrites)
          forM_ rpDraws $ \rpDraw -> do
            (RenderPass rp framebuffers passes) <-
              unsafeGetHandle rpDraw.drawRenderPass
            let framebuffer = framebuffers !! fromIntegral imageIndex
            let
              clearValues = Vector.fromList rpDraw.drawClearValues
              renderStartPos = Vk.Offset2D 0 0
              renderExtent = vkExtent swapchain
              renderPassBeginInfo =
                Vk.RenderPassBeginInfo
                  ()
                  rp
                  framebuffer
                  (Vk.Rect2D renderStartPos renderExtent)
                  clearValues

            Vk.cmdBeginRenderPass
              cmdBuffer renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE

            let
              viewport = Vk.Viewport
                0 -- startX
                0 -- startY
                (fromIntegral $ Extent2D.width renderExtent) -- width
                (fromIntegral $ Extent2D.height renderExtent) -- height
                0 -- min depth
                1 -- max depth
              scissor = Vk.Rect2D renderStartPos renderExtent
            Vk.cmdSetViewport
              cmdBuffer
              0
              (Vector.singleton viewport)
            Vk.cmdSetScissor
              cmdBuffer
              0
              (Vector.singleton scissor)

            forM_ (zip passes rpDraw.drawSubpasses) $ \(subpass, (DrawSubpass draws)) -> do
              Vk.cmdBindPipeline
                cmdBuffer
                Vk.PIPELINE_BIND_POINT_GRAPHICS
                subpass.subpassPipeline

              sets <-
                if null subpass.subpassSetLayouts
                then pure []
                else
                  -- Not recommended to free descriptor sets, just reset pool. So
                  -- we don't setup a destructor here.
                  fmap Vector.toList $ Vk.allocateDescriptorSets
                    (vkDevice ctx)
                    $ Vk.DescriptorSetAllocateInfo
                        ()
                        descPool
                        (Vector.fromList subpass.subpassSetLayouts)

              forM_ draws $ \draw -> do
                (vertexBufs, vertexOffsets)
                  <- drawVertexBuffers draw
                     & mapM handleGet
                     <&> unzip . fmap (\(Buffer buf off _sz) -> (buf, off))
                unless (null vertexBufs) $
                  Vk.cmdBindVertexBuffers
                    cmdBuffer
                    0 -- first binding
                    (Vector.fromList vertexBufs)
                    (Vector.fromList vertexOffsets)
                forM_ (drawIndexBuffers draw) $ \(h, indexType) -> do
                  (Buffer indexBuf offset _sz) <- handleGet h
                  Vk.cmdBindIndexBuffer
                    cmdBuffer
                    indexBuf
                    offset
                    indexType

                unless (null sets) $ do
                  (writes :: [Vk.WriteDescriptorSet '[]]) <-
                    fmap concat $ forM (zip sets (drawDescriptors draw)) $ \(set, ds) ->
                      fmap concat $ forM (zip [0..] ds) $ \(ix, descriptor) ->
                        case descriptor of
                          (DescriptorUniform (Buffer buf off sz)) ->
                            pure [ Vk.WriteDescriptorSet
                                     ()
                                     set -- dst set
                                     ix -- dst binding
                                     0 -- dst array element
                                     1 -- descriptor count
                                     Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER -- type
                                     mempty -- image info
                                     -- buffer info
                                     ( Vector.singleton $
                                         Vk.DescriptorBufferInfo
                                           buf
                                           off -- offset
                                           sz
                                     )
                                     mempty -- texel buffer view
                                 ]
                          (DescriptorTexture (TextureHandle th)) -> do
                            (Texture _img imgView sampler) <- unsafeGet th
                            pure [ Vk.WriteDescriptorSet
                                     ()
                                     set -- dst set
                                     ix -- dst binding
                                     0 -- dst array element
                                     1 -- descriptor count
                                     Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER -- type
                                     ( Vector.singleton $ Vk.DescriptorImageInfo
                                         sampler
                                         imgView
                                         Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                     ) -- image info
                                     mempty -- buffer info
                                     mempty -- texel buffer view
                                 ]

                  liftIO $ Vk.updateDescriptorSets (vkDevice ctx)
                    (fmap Vk.SomeStruct . Vector.fromList $ writes)
                    -- copies
                    mempty

                  Vk.cmdBindDescriptorSets
                    cmdBuffer
                    Vk.PIPELINE_BIND_POINT_GRAPHICS
                    subpass.subpassPipelineLayout
                    0 -- first set
                    (Vector.fromList sets)
                    mempty -- dynamic offsets

                forM (drawCall draw) $
                  \case
                    (IndexedDraw (DrawCallIndexed indexCount instanceCount firstIndex vertexOffset firstInstance)) ->
                      Vk.cmdDrawIndexed
                        cmdBuffer indexCount instanceCount firstIndex vertexOffset firstInstance
                    (PrimitiveDraw (DrawCallPrimitive firstVertex firstInstance instanceCount vertexCount)) ->
                      Vk.cmdDraw
                        cmdBuffer vertexCount instanceCount firstVertex firstInstance

            Vk.cmdEndRenderPass cmdBuffer
          Vk.endCommandBuffer cmdBuffer
          pure cmdBuffer
  case result of
    Left SwapchainOutOfDate ->
      Swapchain.recreateSwapchain ctx r.rendererSwapchain $ \newSc -> do
        void $ mask $ \restore -> do
          rps <- liftIO $ STM.atomically $ STM.flushTQueue r.rendererPasses
          restore (mapM (evalRenderPass' (vkAllocator ctx) (vkDevice ctx) newSc) rps)
            `finally` (liftIO $ STM.atomically $ forM_ rps (writeTQueue r.rendererPasses))
    Right x ->
      pure x

texture
   :: (Storable a, MonadIO m)
   => Renderer
   -> Acquire (TextureInfo a)
   -> m (Handle Texture)
texture _ info =
  TextureHandle <$> defer info

evalTexture
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadResource m
     )
  => VkContext -> Renderer -> Handle Texture -> m Texture
evalTexture ctx r (TextureHandle d) = do
  evalEmpty d $ \texInfo ->
    with texInfo $ \(TextureInfo fmt w h sz (imgData :: [x])) -> do
      props <- Vk.getPhysicalDeviceProperties $ vkPhysicalDevice ctx
      let
        imageCreateInfo = Vk.ImageCreateInfo
          ()
          Vk.zero
          Vk.IMAGE_TYPE_2D -- imageType
          Vk.FORMAT_R8G8B8A8_SRGB -- format
          (Vk.Extent3D (fromIntegral w) (fromIntegral h) 1) -- extent
          1 -- mipLevels
          1 -- arrayLayers
          Vk.SAMPLE_COUNT_1_BIT -- samples
          Vk.IMAGE_TILING_OPTIMAL -- tiling
          (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) -- usage
          Vk.SHARING_MODE_EXCLUSIVE -- sharingMode
          Vector.empty -- queueFamilyIndices, ignored because SHARING_MODE not "concurrent"
          Vk.IMAGE_LAYOUT_UNDEFINED -- initialLayout

      -- sampler <- withSampler (vkLogicalDevice ctx) samplerCreateInfo
      (_relKey, imgAlloc) <- allocateAcquire $
        withImage (vkAllocator ctx) imageCreateInfo sz

      withAllocPtr imgAlloc $ \ptr ->
        liftIO $ pokeArray (castPtr @() @x ptr) imgData
      _ <- flush (vkAllocator ctx) imgAlloc 0 sz

      case requiresBufferCopy imgAlloc of
        Nothing -> pure ()
        Just (srcBuf, dstImg) -> do
          liftIO $ STM.atomically $ STM.writeTQueue r.rendererPendingWrites $ do
            let
              imgSubresourceLayers = Vk.ImageSubresourceLayers
                Vk.IMAGE_ASPECT_COLOR_BIT -- aspectMask
                0 -- mipLevel
                0 -- baseArrayLayer
                1 -- layerCount
              region = Vk.BufferImageCopy
                0 -- bufferOffset
                0 -- bufferRowLength :: Word32
                0 -- bufferImageHeight :: Word32
                imgSubresourceLayers -- imageSubresource :: ImageSubresourceLayers
                (Vk.Offset3D 0 0 0) -- imageOffset :: Offset3D
                (Vk.Extent3D (fromIntegral w) (fromIntegral h) 1) -- imageExtent :: Extent3D
            CopyToImage
              $ BufferImageCopyInfo { srcBuffer = srcBuf
                                    , dstImage = dstImg
                                    , regions = Vector.singleton region
                                    }

      let
        imageViewCreateInfo = Vk.ImageViewCreateInfo
          ()
          Vk.zero
          (getAllocData imgAlloc)
          Vk.IMAGE_VIEW_TYPE_2D
          fmt
          (Vk.ComponentMapping Vk.COMPONENT_SWIZZLE_IDENTITY
                               Vk.COMPONENT_SWIZZLE_IDENTITY
                               Vk.COMPONENT_SWIZZLE_IDENTITY
                               Vk.COMPONENT_SWIZZLE_IDENTITY
          )
          (Vk.ImageSubresourceRange
            Vk.IMAGE_ASPECT_COLOR_BIT -- Colour target
            0 -- Base mipmap level
            1 -- Mipmap levels
            0 -- Base layer
            1 -- Layer count
          )

      (_, imgView) <- allocateAcquire $
        withImageView (vkDevice ctx) imageViewCreateInfo

      let
        samplerCreateInfo = Vk.SamplerCreateInfo
          ()
          Vk.zero
          Vk.FILTER_LINEAR -- mag filter
          Vk.FILTER_LINEAR -- min filter
          Vk.SAMPLER_MIPMAP_MODE_LINEAR -- mipmap mode
          Vk.SAMPLER_ADDRESS_MODE_REPEAT -- address mode u
          Vk.SAMPLER_ADDRESS_MODE_REPEAT -- address mode v
          Vk.SAMPLER_ADDRESS_MODE_REPEAT -- address mode w
          0 -- mip lod bias
          True -- anisotropy enable
          (Vk.maxSamplerAnisotropy $ Vk.limits props) -- max anisotropy
          False -- compare enable
          Vk.COMPARE_OP_ALWAYS -- compare op
          0 -- min lod
          1 -- max lod (mip levels)
          Vk.BORDER_COLOR_INT_OPAQUE_BLACK -- border color
          False -- unnormalized coordinates
      (_, sampler) <- allocateAcquire $
        withSampler (vkDevice ctx) samplerCreateInfo

      pure $ Texture (getAllocData imgAlloc) imgView sampler

shader
   :: MonadIO m
   => Renderer
   -> Vk.ShaderStageFlagBits
   -- Shader stage (vertex, fragment, etc.)
   -> BS.ByteString
   -- ^ Shader entry function name
   -> Acquire BSL.ByteString
   -- ^ Shader data
   -> m (Handle Shader)
shader _ stage entry code =
  ShaderHandle <$> defer (ShaderInfo stage entry code)

buffer
  :: (MonadResource m, MonadIO m)
  => VkContext
  -> Renderer
  -> Acquire a
  -> (a -> Load b)
  -> m (NewHandle b)
buffer ctx r acquire load =
  mkHandle $ do
    (b, copies) <- with acquire $ \a -> do
      fmap snd $
        allocateAcquire $
          flip runStateT [] $
          flip runReaderT (vkAllocator ctx) $
            load a
    liftIO $ STM.atomically $ forM copies $ STM.writeTQueue r.rendererPendingWrites
    pure b

renderPass
  :: MonadIO m
  => Renderer
  -> RenderPassInfo
  -> m (Handle RenderPass)
renderPass r info = do
  h <- RenderPassHandle <$> defer info
  liftIO $ STM.atomically $ writeTQueue (rendererPasses r) h
  pure h

evalRenderPass
  :: (MonadResource m, MonadMask m, MonadUnliftIO m)
  => Allocator
  -> Vk.Device
  -> Swapchain
  -> Handle RenderPass
  -> m RenderPass
evalRenderPass allocator device swapchain (RenderPassHandle d) = do
  (_relKey, l) <- evalEmpty d $ \info -> do
    forM_ info.subpasses $ \subpassInfo ->
      forM_ subpassInfo.shaderStages $ evalShader device
    allocateAcquire $
      acquireRenderPass allocator device swapchain info
  pure l

evalRenderPass'
  :: (MonadResource m, MonadMask m, MonadUnliftIO m)
  => Allocator
  -> Vk.Device
  -> Swapchain
  -> Handle RenderPass
  -> m RenderPass
evalRenderPass' allocator device swapchain (RenderPassHandle d) = do
  (_relKey, l) <- eval d $ \info mOld -> do
    forM_ info.subpasses $ \subpassInfo ->
      forM_ subpassInfo.shaderStages $ evalShader device
    allocateAcquire (acquireRenderPass allocator device swapchain info)
    <* (case mOld of
          Nothing -> pure ()
          Just (oldRel, _) -> release oldRel
       )
  pure l

acquireRenderPass
  :: Allocator
  -> Vk.Device
  -> Swapchain
  -> RenderPassInfo
  -> Acquire RenderPass
acquireRenderPass allocator device swapchain info = do
  rp <- withVkRenderPass device
    $ Vk.RenderPassCreateInfo
       ()
        Vk.zero
        -- attachment descriptions
        ( Vector.fromList $ fmap (toAttachmentDescription swapchain) info.attachments)
        -- subpass descriptions
        ( Vector.fromList $ fmap (.attachmentUsage) info.subpasses )
        -- subpass dependencies
        ( Vector.fromList info.subpassDependencies )

  let
  imageViewFunctions <- forM info.attachments $ \a ->
    case a.format of
      SwapchainColorFormat -> pure id
      SwapchainDepthFormat -> do
        imgAlloc <- withImageAttachment allocator
          $ Vk.ImageCreateInfo
              ()
              Vk.zero
              Vk.IMAGE_TYPE_2D -- imageType
              (vkDepthFormat swapchain) -- format
              (Vk.Extent3D (vkSwapchainWidth swapchain) (vkSwapchainHeight swapchain) 1) -- extent
              1 -- mipLevels
              1 -- arrayLayers
              Vk.SAMPLE_COUNT_1_BIT -- samples
              Vk.IMAGE_TILING_OPTIMAL -- tiling
              Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT -- usage
              Vk.SHARING_MODE_EXCLUSIVE -- sharingMode
              Vector.empty -- queueFamilyIndices, ignored because SHARING_MODE not "concurrent"
              Vk.IMAGE_LAYOUT_UNDEFINED -- initialLayout
        let
          imageViewCreateInfo = Vk.ImageViewCreateInfo
            ()
            Vk.zero
            (getAllocData imgAlloc)
            Vk.IMAGE_VIEW_TYPE_2D
            (vkDepthFormat swapchain)
            (Vk.ComponentMapping Vk.COMPONENT_SWIZZLE_IDENTITY
                                 Vk.COMPONENT_SWIZZLE_IDENTITY
                                 Vk.COMPONENT_SWIZZLE_IDENTITY
                                 Vk.COMPONENT_SWIZZLE_IDENTITY
            )
            (Vk.ImageSubresourceRange
              Vk.IMAGE_ASPECT_DEPTH_BIT -- Colour target
              0 -- Base mipmap level
              1 -- Mipmap levels
              0 -- Base layer
              1 -- Layer count
            )

        imgView <-
          withImageView device imageViewCreateInfo
        pure $ const imgView
      _ -> error "Attachment image format unhandled"


  framebuffers <-
    forM (Vector.toList $ vkImageViews swapchain) $ \iv -> do
      withVkFramebuffer device
        $ Vk.FramebufferCreateInfo
          ()
          Vk.zero
          rp -- render pass
          (Vector.fromList $ fmap ($ iv) imageViewFunctions) -- attachments
          -- dimensions
          (Extent2D.width $ vkExtent swapchain)
          (Extent2D.height $ vkExtent swapchain)
          1 -- layers

  sps <- forM (zip [0..] info.subpasses) $ \(ix, sp) ->
    acquireSubpass device rp sp ix

  pure $ RenderPass rp framebuffers sps

acquireSubpass
  :: Vk.Device
  -> Vk.RenderPass
  -> SubpassInfo
  -> Word32
  -> Acquire Subpass
acquireSubpass device rp subpassInfo subpassIx = do
  -- TODO: Better interface for handles
  shaders <-
    mapM (\(ShaderHandle d) -> unsafeGet d) subpassInfo.shaderStages

  setLayouts <-
    forM subpassInfo.descriptors $ \set ->
      withVkDescriptorSetLayout device
        $ Vk.DescriptorSetLayoutCreateInfo
            ()
            Vk.zero
            (Vector.fromList set)

  pipelineLayout <-
    withVkPipelineLayout device
      $ Vk.PipelineLayoutCreateInfo
          Vk.zero
          -- set layouts
          (Vector.fromList setLayouts)
          -- push constant ranges
          Vector.empty

  let
    pipelineShaderStages = (.pipelineShaderStage) <$> shaders
    pipelineCreateInfo = Vk.GraphicsPipelineCreateInfo () Vk.zero
      (fromIntegral $ length pipelineShaderStages)
      (fmap Vk.SomeStruct . Vector.fromList $ pipelineShaderStages)
      ( Just . Vk.SomeStruct
        $ Vk.PipelineVertexInputStateCreateInfo
            ()
            Vk.zero
            (Vector.fromList subpassInfo.vertexBindings)
            (Vector.fromList subpassInfo.vertexAttributes)
      )
      ( Just
        $ Vk.PipelineInputAssemblyStateCreateInfo
            Vk.zero
            Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST -- Topology
            False -- Primitive restart
      )
      Nothing -- tesselation
      ( Just . Vk.SomeStruct
        $ Vk.PipelineViewportStateCreateInfo
            ()
            Vk.zero
            -- Viewports (dynamic so empty)
            1
            Vector.empty
            -- Scissors (dynamic so empty)
            1
            Vector.empty
      )
      ( Just . Vk.SomeStruct
       $ Vk.PipelineRasterizationStateCreateInfo
           ()
           Vk.zero
           False -- Depth clamp
           False -- Rasterizer discard
           Vk.POLYGON_MODE_FILL -- Polygon mode
           subpassInfo.cullMode -- Cull mode
           subpassInfo.frontFace -- Facing
           False -- Depth bias
           0 -- Depth bias constant factor
           0 -- Depth bias clamp
           0 -- Depth bias slope factor
           1 -- Line width
      )
      ( Just . Vk.SomeStruct
          $ Vk.PipelineMultisampleStateCreateInfo
              ()
              Vk.zero
              Vk.SAMPLE_COUNT_1_BIT -- Number of samples
              False -- Sample shading enable
              1 -- Min sample shading
              Vector.empty -- Sample mask
              False -- Alpha to coverage
              False -- Alpha to one
      )
      ( Just
          $ Vk.PipelineDepthStencilStateCreateInfo
              Vk.zero
              True -- depth test enable
              True -- depth write enable
              Vk.COMPARE_OP_LESS -- compare op
              False -- bounds test
              False -- stencil test enable
              Vk.zero -- front stencil
              Vk.zero -- back stencil
              0 -- min depth
              1 -- max depth
      )
      ( Just . Vk.SomeStruct
          $ Vk.PipelineColorBlendStateCreateInfo
              ()
              Vk.zero
              False -- Logic op enable
              Vk.LOGIC_OP_COPY -- logic op
              1 -- attachment count
              -- attachments
              ( Vector.singleton
                $ Vk.PipelineColorBlendAttachmentState
                    False -- blend enable
                    Vk.zero -- src color blend factor
                    Vk.zero -- dst color blend factor
                    Vk.zero -- color blend op
                    Vk.zero -- src alpha blend factor
                    Vk.zero -- dst alpha blend factor
                    Vk.zero -- alpha blend op
                    -- color write mask
                    (Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT)
              )
              (0, 0, 0, 0) -- Blend constants
      )
      ( Just
          $ Vk.PipelineDynamicStateCreateInfo
              Vk.zero
              (Vector.fromList [ Vk.DYNAMIC_STATE_VIEWPORT
                               , Vk.DYNAMIC_STATE_SCISSOR
                               ]
              )
      )
      pipelineLayout
      rp
      subpassIx -- subpass index
      Vk.NULL_HANDLE -- pipeline handle (inheritance)
      (-1) -- pipeline index (inheritance)

  pipeline <-
    Vector.head <$> withVkGraphicsPipelines
      device
      Vk.NULL_HANDLE
      (Vector.singleton pipelineCreateInfo)
  pure $ Subpass pipeline pipelineLayout setLayouts

evalBuffer
  :: (MonadUnliftIO m, MonadMask m, MonadResource m)
  => VkContext
  -> Renderer
  -> NewHandle Buffer
  -> m Buffer
evalBuffer ctx r h = do
  handleGet h

evalShader
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadResource m
     )
  => Vk.Device -> Handle Shader -> m Shader
evalShader device (ShaderHandle d) = do
  evalEmpty d $ \createInfo -> do
    with createInfo.dat $ \shaderCode -> do
      (_relKey, shaderModule) <- allocateAcquire $
        withVkShaderModule device $
          Vk.ShaderModuleCreateInfo () Vk.zero (BSL.toStrict shaderCode)

      pure
        $ Shader
        $ Vk.PipelineShaderStageCreateInfo
            ()
            Vk.zero
            createInfo.shaderStage
            shaderModule
            createInfo.entryFn
            Nothing

finallyPresent
  :: MonadIO m
  => Vk.Device
  -> Vk.Queue
  -- ^ Graphics queue
  -> Vk.Queue
  -- ^ Present queue
  -> Vk.SwapchainKHR
  -> FrameSync
  -> (("imageIndex" Vk.::: Word32) -> m Vk.CommandBuffer)
  -> m ()
finallyPresent device gfxQue presentQue swapchain fs f = do
  void $ Vk.waitForFences
    device
    (Vector.singleton $ fsFenceInFlight fs)
    True
    maxBound

  (_, imageIndex) <- liftIO $ throwSwapchainOutOfDate $
    Vk.acquireNextImageKHR
      device
      swapchain
      maxBound
      (fsSemaphoreImageAvailable fs)
      Vk.NULL_HANDLE

  -- Only reset fences if we know we are submitting work, otherwise we
  -- might deadlock later while waiting for the fence to signal.
  liftIO $ Vk.resetFences
    device
    (Vector.singleton $ fsFenceInFlight fs)

  cmdBuffer <- f imageIndex

  -- TODO: Maybe don't submit command buffer on behalf of user?
  let
    submitInfo =
      Vk.SubmitInfo
        ()
        (Vector.fromList $ [fsSemaphoreImageAvailable fs])
        (Vector.fromList $ [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT])
        (Vector.singleton $ Vk.commandBufferHandle cmdBuffer)
        (Vector.singleton $ fsSemaphoreRenderFinished fs)
  liftIO $ Vk.queueSubmit
    gfxQue
    (Vector.singleton $ Vk.SomeStruct submitInfo)
    (fsFenceInFlight fs)

  let
    presentInfo = Vk.PresentInfoKHR
      ()
      (Vector.singleton $ fsSemaphoreRenderFinished fs)
      (Vector.singleton swapchain)
      (Vector.singleton imageIndex)
      nullPtr
  liftIO $ void
    $ (throwSwapchainSubOptimal =<<)
    $ throwSwapchainOutOfDate
    $ Vk.queuePresentKHR presentQue presentInfo
