{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rort.Render where

import Rort.Vulkan.Context ( VkContext, VkContext(..), vkGraphicsQueue, vkPresentationQueue, vkTransferQueue, vkTransferQueueIx, vkGraphicsQueueIx )
import Rort.Render.Swapchain (Swapchain, vkSurfaceFormat, vkImageViews, vkExtent, throwSwapchainOutOfDate, throwSwapchainSubOptimal, vkSwapchain, SwapchainOutOfDate (SwapchainOutOfDate))
import Rort.Render.Types ( DrawCall(PrimitiveDraw, IndexedDraw), Subpass(Subpass), DrawCallPrimitive(..), SubpassInfo(..), Draw(..), DrawCallIndexed(..), Shader(Shader), pipelineShaderStage, Handle (ShaderHandle, BufferHandle, RenderPassLayoutHandle, SubpassHandle), ShaderInfo (..), Buffer(Buffer), BufferInfo (..), RenderPassLayout (RenderPassLayout), RenderPassLayoutInfo (RenderPassLayoutInfo), unsafeGetHandle )
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.Vector as Vector
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import Rort.Vulkan (withVkDescriptorSetLayout, withVkRenderPass, withVkGraphicsPipelines, withVkFramebuffer, withVkPipelineLayout, withVkShaderModule, withVkCommandBuffers, withSemaphore)
import qualified Vulkan.Extensions.VK_KHR_surface as VkFormat
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
import Rort.Util.Defer (defer, evalEmpty, getSeed, eval, unsafeGet)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO, runResourceT, release)
import Control.Exception.Safe (MonadMask, try, finally, mask, onException)
import Rort.Allocator (withBuffer, withAllocPtr, flush, getAllocBuffer, getAllocOffset, requiresBufferCopy)
import Control.Concurrent.STM (TQueue, newTQueueIO, writeTQueue)
import qualified Control.Concurrent.STM as STM
import Rort.Render.SwapchainImages (SwapchainImages, SwapchainImage (SwapchainImage))
import qualified Rort.Render.SwapchainImages as Swapchain
import Data.Vector (Vector)
import Data.Function ((&))

data Renderer
  = Renderer { rendererSwapchain      :: SwapchainImages
             , rendererFramesInFlight :: FramesInFlight
             , rendererLayouts        :: TQueue (Handle RenderPassLayout)
             , rendererPasses         :: TQueue (Handle Subpass)
             , rendererDevice         :: Vk.Device
             , rendererPendingWrites  :: TQueue BufferCopyInfo
             }

data BufferCopyInfo = BufferCopyInfo { srcBuffer    :: Vk.Buffer
                                     , dstBuffer    :: Vk.Buffer
                                     , regions      :: Vector Vk.BufferCopy
                                     , dstStageMask :: Vk.PipelineStageFlagBits2
                                     }

recordBufferCopies
  :: MonadResource m
  => VkContext
  -> Vk.CommandPool
  -> Vk.CommandBuffer
  -> [BufferCopyInfo]
  -> m ()
recordBufferCopies _ctx _cmdPool _cmdBuffer [] = pure ()
recordBufferCopies ctx cmdPool cmdBuffer bufferCopyInfos = do
  forM_ bufferCopyInfos $ \copy ->
    Vk.cmdCopyBuffer cmdBuffer copy.srcBuffer copy.dstBuffer copy.regions

  if vkGraphicsQueue ctx == vkTransferQueue ctx
  then do
    let
      barriers = flip foldMap bufferCopyInfos $ \copy ->
        [ Vk.MemoryBarrier2
            Vk.PIPELINE_STAGE_2_TRANSFER_BIT_KHR -- src stage mask
            Vk.ACCESS_2_MEMORY_WRITE_BIT_KHR -- src access mask
            copy.dstStageMask
            Vk.ACCESS_2_MEMORY_READ_BIT_KHR -- dst access mask
        ]

      dependencyInfo = Vk.DependencyInfo
        Vk.zero                    -- dependency flags
        (Vector.fromList barriers) -- memory barriers
        Vector.empty               -- buffer memory barriers
        Vector.empty               -- image memory barriers

    Vk.cmdPipelineBarrier2KHR cmdBuffer dependencyInfo
  else do
    _ <- error "Separate graphics and transfer queue not implemented!"
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
      dependencyInfo2 = Vk.DependencyInfo
        Vk.zero -- dependency flags
        Vector.empty -- memory barriers
        (Vector.fromList barriers2) -- buffer memory barriers
        Vector.empty -- image memory barriers

    Vk.cmdPipelineBarrier2KHR cmdBuffer dependencyInfo2

createRenderer
  :: MonadResource m
  => VkContext
  -> NumFramesInFlight
  -> m Renderer
createRenderer ctx numFramesInFlight = do
  swapchainImgs <- Swapchain.create ctx

  framesInFlight <-
    withFramesInFlight (vkDevice ctx) (vkQueueFamilies ctx) numFramesInFlight

  layoutsQue <- liftIO newTQueueIO
  passesQue <- liftIO newTQueueIO

  pendingWrites <- liftIO newTQueueIO

  pure $
    Renderer
      swapchainImgs
      framesInFlight
      layoutsQue
      passesQue
      (vkDevice ctx)
      pendingWrites

submit
  :: ( MonadResource m
     , MonadMask m
     , MonadUnliftIO m
     )
  => VkContext
  -> Renderer
  -> (Swapchain -> m [Draw])
  -> m ()
submit ctx r getDraws = do
  result <- try $ Swapchain.withSwapchainImage r.rendererSwapchain $ \(SwapchainImage _ swapchain) -> do
    draws <- getDraws swapchain
    forM_ draws $ \draw -> do
      let
        s@(SubpassHandle h) = drawSubpass draw
        subpassInfo = getSeed h
      _ <- evalRenderPassLayout (vkDevice ctx) swapchain subpassInfo.layout
      _ <- evalSubpass (vkDevice ctx) swapchain s
      let buffers = drawVertexBuffers draw <> fmap fst (drawIndexBuffers draw)
      mapM_ (evalBuffer ctx r) buffers
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
            restore (recordBufferCopies ctx cmdPool cmdBuffer bufferCopies)
              `onException` liftIO (STM.atomically $ forM_ bufferCopies $ STM.writeTQueue r.rendererPendingWrites)
          forM_ draws $ \draw -> do
            let
              (SubpassHandle h) = draw.drawSubpass
              subpassInfo = getSeed h
            (RenderPassLayout rp framebuffers) <-
              unsafeGetHandle subpassInfo.layout
            (Subpass pipeline pipelineLayout setLayouts) <-
              unsafeGetHandle draw.drawSubpass
            let framebuffer = framebuffers !! fromIntegral imageIndex
            let
              clearValues = Vector.fromList [Vk.Color $ Vk.Float32 0 0 0 0]
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

            Vk.cmdBindPipeline
              cmdBuffer
              Vk.PIPELINE_BIND_POINT_GRAPHICS
              pipeline

            (vertexBufs, vertexOffsets)
              <- drawVertexBuffers draw
                 & mapM (\(BufferHandle d) -> unsafeGet d)
                 <&> unzip . fmap (\(Buffer buf off _sz) -> (buf, off))
            unless (null vertexBufs) $
              Vk.cmdBindVertexBuffers
                cmdBuffer
                0 -- first binding
                (Vector.fromList vertexBufs)
                (Vector.fromList vertexOffsets)
            forM_ (drawIndexBuffers draw) $ \(BufferHandle d, indexType) -> do
              (Buffer indexBuf offset _sz) <- unsafeGet d
              Vk.cmdBindIndexBuffer
                cmdBuffer
                indexBuf
                offset
                indexType

            unless (null setLayouts) $ do
              sets <-
                -- Not recommended to free descriptor sets, just reset pool. So
                -- we don't setup a destructor here.
                fmap Vector.toList $ Vk.allocateDescriptorSets
                  (vkDevice ctx)
                  $ Vk.DescriptorSetAllocateInfo
                      ()
                      descPool
                      (Vector.fromList setLayouts)
              let
                writes = flip foldMap (zip sets (drawUniformBuffers draw)) $
                  \(set, Buffer buf off sz) ->
                    [ Vk.WriteDescriptorSet
                        ()
                        set -- dst set
                        0 -- dst binding
                        0 -- dst array element
                        1 -- descriptor count
                        Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
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

              liftIO $ Vk.updateDescriptorSets (vkDevice ctx)
                (fmap Vk.SomeStruct . Vector.fromList $ writes)
                -- copies
                mempty

              Vk.cmdBindDescriptorSets
                cmdBuffer
                Vk.PIPELINE_BIND_POINT_GRAPHICS
                pipelineLayout
                0 -- first set
                (Vector.fromList sets)
                mempty -- dynamic offsets

            case drawCall draw of
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
          rps <- liftIO $ STM.atomically $ STM.flushTQueue r.rendererLayouts
          restore (mapM (evalRenderPassLayout' (vkDevice ctx) newSc) rps)
            `finally` (liftIO $ STM.atomically $ forM_ rps (writeTQueue r.rendererLayouts))
        void $ mask $ \restore -> do
          ps <- liftIO $ STM.atomically $ STM.flushTQueue r.rendererPasses
          restore (mapM (evalSubpass (vkDevice ctx) newSc) ps)
            `finally` (liftIO $ STM.atomically $ forM_ ps (writeTQueue r.rendererPasses))
    Right x ->
      pure x

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
  :: (MonadIO m, Storable a)
  => Renderer
  -> Vk.BufferUsageFlagBits
  -- ^ Buffer usage (vertex, index, etc.)
  -> Acquire (Word64, [a])
  -- ^ (buffer size, buffer data)
  -> m (Handle Buffer)
buffer _ use bufferDat =
  BufferHandle <$> defer (BufferInfo use bufferDat)

renderPassLayout
  :: MonadIO m
  => Renderer
  -> m (Handle RenderPassLayout)
renderPassLayout r = do
  h <- RenderPassLayoutHandle <$> defer RenderPassLayoutInfo
  liftIO $ STM.atomically $ writeTQueue (rendererLayouts r) h
  pure h

subpass
  :: MonadIO m
  => Renderer
  -> SubpassInfo
  -> m (Handle Subpass)
subpass r subpassInfo = do
  h <- SubpassHandle <$> defer subpassInfo
  liftIO $ STM.atomically $ writeTQueue (rendererPasses r) h
  pure h

evalRenderPassLayout
  :: (MonadResource m, MonadMask m)
  => Vk.Device
  -> Swapchain
  -> Handle RenderPassLayout
  -> m RenderPassLayout
evalRenderPassLayout device swapchain (RenderPassLayoutHandle d) = do
  (_relKey, l) <- evalEmpty d $ \RenderPassLayoutInfo -> allocateAcquire $ do
    acquireRenderPassLayout device swapchain
  pure l

evalRenderPassLayout'
  :: (MonadResource m, MonadMask m)
  => Vk.Device
  -> Swapchain
  -> Handle RenderPassLayout
  -> m RenderPassLayout
evalRenderPassLayout' device swapchain (RenderPassLayoutHandle d) = do
  (_relKey, l) <- eval d $ \RenderPassLayoutInfo mOld ->
    allocateAcquire (acquireRenderPassLayout device swapchain)
    <* (case mOld of
          Nothing -> pure ()
          Just (oldRel, _) -> release oldRel
       )
  pure l

acquireRenderPassLayout :: Vk.Device -> Swapchain -> Acquire RenderPassLayout
acquireRenderPassLayout device swapchain = do
  rp <- withVkRenderPass device
    $ Vk.RenderPassCreateInfo
       ()
        Vk.zero
        -- attachment descriptions
        ( Vector.singleton $ Vk.AttachmentDescription
            Vk.zero
            (VkFormat.format $ vkSurfaceFormat swapchain)
            Vk.SAMPLE_COUNT_1_BIT            -- Samples
            Vk.ATTACHMENT_LOAD_OP_CLEAR      -- Load op, what to do with framebuffer before rendering
            Vk.ATTACHMENT_STORE_OP_STORE     -- Store op, store the framebuffer
            Vk.ATTACHMENT_LOAD_OP_DONT_CARE  -- Stencil load op
            Vk.ATTACHMENT_STORE_OP_DONT_CARE -- Stencil store op
            Vk.IMAGE_LAYOUT_UNDEFINED        -- Initial layout
            Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR  -- Final layout
        )
        -- subpass descriptions
        ( Vector.singleton $ Vk.SubpassDescription
            Vk.zero
            Vk.PIPELINE_BIND_POINT_GRAPHICS
            Vector.empty -- input attachments
            -- color attachments
            ( Vector.singleton $ Vk.AttachmentReference
                0 -- attachment ix
                Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
            )
            Vector.empty -- resolve attachments
            Nothing      -- depth stencil attachments
            Vector.empty -- preserve attachments
        )
        -- subpass dependencies
        ( Vector.singleton $ Vk.SubpassDependency
            Vk.SUBPASS_EXTERNAL -- src subpass
            0 -- dst subpass
            Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT -- src stage mask
            Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT -- dst stage mask
            Vk.zero -- src access mask
            Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT -- dst access mask
            Vk.zero -- dependency flags
        )

  framebuffers <-
    forM (Vector.toList $ vkImageViews swapchain) $ \iv -> do
      withVkFramebuffer device
        $ Vk.FramebufferCreateInfo
          ()
          Vk.zero
          rp -- render pass
          (Vector.singleton iv) -- attachments
          -- dimensions
          (Extent2D.width $ vkExtent swapchain)
          (Extent2D.height $ vkExtent swapchain)
          1 -- layers
  pure $ RenderPassLayout rp framebuffers

evalSubpass
  :: (MonadMask m, MonadUnliftIO m, MonadResource m)
  => Vk.Device
  -> Swapchain
  -> Handle Subpass
  -> m Subpass
evalSubpass device swapchain (SubpassHandle d) = do
  (_relKey, pass) <- evalEmpty d $ \subpassInfo -> do
    (RenderPassLayout rp _framebuffers) <-
      evalRenderPassLayout device swapchain subpassInfo.layout
    shaders <-
      mapM (evalShader device) subpassInfo.shaderStages
    allocateAcquire $ acquireSubpass device subpassInfo shaders rp
  pure pass

evalSubpass'
  :: (MonadMask m, MonadUnliftIO m, MonadResource m)
  => Vk.Device
  -> Swapchain
  -> Handle Subpass
  -> m Subpass
evalSubpass' device swapchain (SubpassHandle d) = do
  (_relKey, pass) <- eval d $ \subpassInfo mOld -> do
    (RenderPassLayout rp _framebuffers) <-
      evalRenderPassLayout device swapchain subpassInfo.layout
    shaders <-
      mapM (evalShader device) subpassInfo.shaderStages
    allocateAcquire (acquireSubpass device subpassInfo shaders rp)
      <* (case mOld of
            Nothing -> pure ()
            Just (oldRel, _) -> release oldRel
         )
  pure pass

acquireSubpass
  :: Vk.Device
  -> SubpassInfo
  -> [Shader]
  -> Vk.RenderPass
  -> Acquire Subpass
acquireSubpass device subpassInfo shaders rp = do
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
           Vk.CULL_MODE_NONE -- Cull mode
           Vk.FRONT_FACE_CLOCKWISE -- Facing
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
      subpassInfo.subpassIndex -- subpass index
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
  -> Handle Buffer
  -> m Buffer
evalBuffer ctx r (BufferHandle d) = do
  evalEmpty d $ \createInfo ->
    with createInfo.dat $ \(sz, storable :: [x]) -> do
      (_relKey, alloc) <- allocateAcquire $
        withBuffer (vkAllocator ctx) createInfo.usage sz
      withAllocPtr alloc $ \ptr ->
        liftIO $ pokeArray (castPtr @() @x ptr) storable
      _ <- flush (vkAllocator ctx) alloc 0 sz
      case requiresBufferCopy alloc of
        Nothing -> pure ()
        Just (srcBuf, dstBuf) -> do
          liftIO $ STM.atomically $ STM.writeTQueue r.rendererPendingWrites $
            BufferCopyInfo { srcBuffer = srcBuf
                           , dstBuffer = dstBuf
                           , regions = Vector.singleton $
                               Vk.BufferCopy 0 -- src offset
                                             0 -- dst offset
                                             sz
                           , dstStageMask = mkDstStageMask createInfo.usage
                           }
      pure $ Buffer (getAllocBuffer alloc) (getAllocOffset alloc) sz

mkDstStageMask :: Vk.BufferUsageFlagBits -> Vk.PipelineStageFlags2
mkDstStageMask use =
  let
    vertexDstStageMask =
      if use .&. Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT == Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
      then (.|. Vk.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT)
      else id
    indexDstStageMask =
      if use .&. Vk.BUFFER_USAGE_INDEX_BUFFER_BIT == Vk.BUFFER_USAGE_INDEX_BUFFER_BIT
      then (.|. Vk.PIPELINE_STAGE_2_INDEX_INPUT_BIT)
      else id
  in
    indexDstStageMask . vertexDstStageMask $ Vk.PIPELINE_STAGE_2_NONE


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
        (Vector.singleton $ fsSemaphoreImageAvailable fs)
        (Vector.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
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
