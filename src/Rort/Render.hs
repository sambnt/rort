{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rort.Render where

import Rort.Vulkan.Context ( VkContext, VkContext(..), vkTransferQueueIx, vkGraphicsQueueIx, vkGraphicsQueue, vkPresentationQueue, vkTransferQueue )
import Rort.Render.Swapchain (Swapchain, vkSurfaceFormat, vkImageViews, vkExtent, throwSwapchainOutOfDate, throwSwapchainSubOptimal, withSwapchain, vkSwapchain, SwapchainOutOfDate (..))
import Rort.Render.Types ( Shader(..), Handle (ShaderHandle, BufferHandle, RenderPassLayoutHandle, SubpassHandle), ShaderInfo (..), Buffer(Buffer), BufferInfo (..), RenderPassLayout(RenderPassLayout), RenderPassLayoutInfo (RenderPassLayoutInfo), Subpass(Subpass), SubpassInfo(..), Draw, DrawCall (..), DrawCallPrimitive (..), DrawCallIndexed (..), drawCall, unsafeGetHandle, drawSubpass, drawVertexBuffers, drawIndexBuffers)
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.Vector as Vector
import Rort.Vulkan (withVkShaderModule, withVkRenderPass, withVkFramebuffer, withVkPipelineLayout, withVkGraphicsPipelines, withVkCommandBuffers)
import qualified Vulkan.CStruct.Extends as Vk
import Data.Functor (void, ($>), (<&>))
import Rort.Render.FramesInFlight (FrameSync (..), FramesInFlight, withFramesInFlight, NumFramesInFlight, FrameInFlight (FrameInFlight), withNextFrameInFlight)
import Data.Word (Word32, Word64, Word8)
import Control.Monad.IO.Class (MonadIO (..))
import Foreign (nullPtr, castPtr, pokeArray, poke, Storable)
import Data.Acquire (Acquire, allocateAcquire, with)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Rort.Util.Defer (defer, evalEmpty, getSeed, eval, unsafeGet)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO, ReleaseKey, runResourceT, release)
import Control.Exception.Safe (MonadMask, try, mask, onException, mask_)
import Rort.Allocator (withBuffer, withAllocPtr, flush, getAllocBuffer, getAllocOffset, requiresBufferCopy)
import Control.Concurrent.STM (TMVar, TQueue, newTMVarIO, newTQueueIO, writeTQueue, flushTQueue)
import qualified Control.Concurrent.STM as STM
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import qualified Vulkan.Extensions.VK_KHR_surface as VkFormat
import Control.Monad (forM, forM_, unless, when)
import Data.Bits ((.|.))
import Rort.Render.SwapchainData (SwapchainData, SwapchainImage (..))
import qualified Rort.Render.SwapchainData as Swapchain
import Data.Function ((&))
import Data.Vector (Vector)

data Renderer
  = Renderer { rendererSwapchain      :: SwapchainData
             , rendererFramesInFlight :: FramesInFlight
             , rendererLayouts        :: TQueue (Handle RenderPassLayout)
             , rendererPasses         :: TQueue (Handle Subpass)
             , rendererDevice         :: Vk.Device
             , rendererPendingWrites  :: TQueue BufferCopyInfo
             }

data BufferCopyInfo = BufferCopyInfo { src          :: Vk.Buffer
                                     , dst          :: Vk.Buffer
                                     , regions      :: Vector Vk.BufferCopy
                                     , dstStageMask :: Vk.PipelineStageFlagBits2
                                     }
  deriving (Eq, Show)

recordBufferCopies
  :: MonadResource m
  => VkContext
  -> Vk.CommandPool
  -> [BufferCopyInfo]
  -> m ()
recordBufferCopies ctx cmdPool [] = pure ()
recordBufferCopies ctx cmdPool bufferCopyInfos = do
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
  forM bufferCopyInfos $ \copy -> do
    Vk.cmdCopyBuffer cmdBuffer copy.src copy.dst copy.regions
  let combinedDstStageMask = foldl (.|.) Vk.zero (dstStageMask <$> bufferCopyInfos)

  if vkGraphicsQueue ctx == vkTransferQueue ctx
  then do
    let
      memoryBarrier = Vk.MemoryBarrier2
        Vk.PIPELINE_STAGE_2_TRANSFER_BIT_KHR -- src stage mask
        Vk.ACCESS_2_MEMORY_WRITE_BIT_KHR -- src access mask
        combinedDstStageMask -- dst stage mask, TODO: Finer grained barrier based on type of allocation
        Vk.ACCESS_2_MEMORY_READ_BIT_KHR -- dst access mask

      dependencyInfo = Vk.DependencyInfo
        Vk.zero -- dependency flags
        (Vector.singleton memoryBarrier) -- memory barriers
        Vector.empty -- buffer memory barriers
        Vector.empty -- image memory barriers

    Vk.cmdPipelineBarrier2KHR cmdBuffer dependencyInfo

    Vk.endCommandBuffer cmdBuffer

    let
      submitInfo = Vk.SubmitInfo
        () -- next
        Vector.empty -- wait semaphores
        Vector.empty -- wait dst stage mask
        (Vector.singleton $ Vk.commandBufferHandle cmdBuffer)
        Vector.empty -- signal semaphores

    Vk.queueSubmit (vkGraphicsQueue ctx) (Vector.singleton $ Vk.SomeStruct submitInfo) Vk.NULL_HANDLE
  else do
    -- let
    --   bufferMemoryBarrier = Vk.BufferMemoryBarrier2
    --     Vk.PIPELINE_STAGE_2_TRANSFER_BIT_KHR -- src stage mask
    --     Vk.ACCESS_2_MEMORY_WRITE_BIT_KHR -- src access mask
    --     Vk.zero -- dst stage mask
    --     Vk.zero -- dst access mask
    --     (vkTransferQueueIx ctx) -- src queue family ix
    --     (vkGraphicsQueueIx ctx) -- dst queue family ix
    --     deviceBuf -- buffer
    --     deviceAllocInfo.offset -- offset
    --     deviceAllocInfo.size -- size

    --   dependencyInfo = Vk.DependencyInfo
    --     Vk.zero -- dependency flags
    --     Vector.empty -- memory barriers
    --     (Vector.singleton bufferMemoryBarrier) -- buffer memory barriers
    --     Vector.empty -- image memory barriers

    -- Vk.cmdPipelineBarrier2KHR cmdBuffer dependencyInfo

    -- Vk.endCommandBuffer cmdBuffer

    -- let
    --   submitInfo = Vk.SubmitInfo
    --     () -- next
    --     Vector.empty -- wait semaphores
    --     Vector.empty -- wait dst stage mask
    --     (Vector.singleton $ Vk.commandBufferHandle cmdBuffer)
    --     Vector.empty -- signal semaphores
    error "unhandled separate transfer and graphics queue"


createRenderer
  :: MonadResource m
  => VkContext
  -> NumFramesInFlight
  -> m Renderer
createRenderer ctx numFramesInFlight = do
  swapchain <- Swapchain.create ctx

  framesInFlight <-
    withFramesInFlight (vkDevice ctx) (vkQueueFamilies ctx) numFramesInFlight

  layoutsQue <- liftIO newTQueueIO
  passesQue <- liftIO newTQueueIO

  writesQue <- liftIO newTQueueIO

  pure $
    Renderer swapchain
             framesInFlight
             layoutsQue
             passesQue
             (vkDevice ctx)
             writesQue

submit :: (MonadMask m, MonadResource m, MonadUnliftIO m) => VkContext -> Renderer -> IO [Draw] -> m ()
submit ctx r f = do
  result <- try $ Swapchain.withSwapchainImage r.rendererSwapchain $ \(SwapchainImage _ swapchain) -> do
    draws <- liftIO f
    forM draws $ \draw -> do
      let
        subpass@(SubpassHandle h) = drawSubpass draw
        subpassInfo = getSeed h
      (RenderPassLayout rp framebuffers) <- evalRenderPassLayout r swapchain subpassInfo.layout
      (Subpass pipeline _pipelineLayout) <- evalSubpass r swapchain subpass
      let buffers = drawVertexBuffers draw <> fmap fst (drawIndexBuffers draw)
      mapM_ (evalBuffer ctx r) buffers
      withNextFrameInFlight
        (vkDevice ctx)
        (rendererFramesInFlight r)
        $ \(FrameInFlight fs _descPool cmdPool) -> runResourceT $ do
          mask_ $ do
            bufferCopies <- liftIO $ STM.atomically $ STM.flushTQueue r.rendererPendingWrites
            recordBufferCopies ctx cmdPool bufferCopies

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
                 <&> unzip . fmap (\(Buffer buf off) -> (buf, off))

            -- liftIO $ putStrLn $ show (vertexBufs, vertexOffsets)
            unless (null vertexBufs) $
              Vk.cmdBindVertexBuffers
                cmdBuffer
                0 -- first binding
                (Vector.fromList vertexBufs)
                (Vector.fromList vertexOffsets)
            forM_ (drawIndexBuffers draw) $ \(BufferHandle d, indexType) -> do
              (Buffer indexBuf offset) <- unsafeGet d
              -- liftIO $ putStrLn $ show (indexBuf,  offset)
              Vk.cmdBindIndexBuffer
                cmdBuffer
                indexBuf
                offset
                indexType

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
    -- get next frame in flight
    -- get framebuffer for swapchain image
    -- allocate command buffer
    -- record draw
  case result of
    Left SwapchainOutOfDate ->
      Swapchain.recreateSwapchain ctx r.rendererSwapchain $ \newSwapchain -> do
        mask $ \restore -> do
          rps <- liftIO $ STM.atomically $ flushTQueue r.rendererLayouts
          rps' <- restore (mapM (\h -> evalRenderPassLayout' r newSwapchain h $> h) rps)
            `onException` liftIO (STM.atomically $ forM_ rps (writeTQueue r.rendererLayouts))
          liftIO $ STM.atomically $ forM_ rps' (writeTQueue r.rendererLayouts)
        mask $ \restore -> do
          ps <- liftIO $ STM.atomically $ flushTQueue r.rendererPasses
          ps' <- restore (mapM (\h -> evalSubpass' r newSwapchain h $> h) ps)
            `onException` liftIO (STM.atomically $ forM_ ps (writeTQueue r.rendererPasses))
          liftIO $ STM.atomically $ forM_ ps' (writeTQueue r.rendererPasses)
    Right _ ->
      pure ()

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
  :: MonadIO m
  => Storable x
  => Renderer
  -> Vk.BufferUsageFlagBits
  -- ^ Buffer usage (vertex, index, etc.)
  -> Acquire (Word64, [x])
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

acquireSubpass :: Renderer -> SubpassInfo -> [Shader] -> Vk.RenderPass -> Acquire Subpass
acquireSubpass r subpassInfo shaders rp = do
  pipelineLayout <-
    withVkPipelineLayout (rendererDevice r)
      $ Vk.PipelineLayoutCreateInfo
          Vk.zero
          -- set layouts
          (Vector.fromList subpassInfo.descriptors)
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
            (Vector.fromList $ subpassInfo.vertexBindings)
            (Vector.fromList $ subpassInfo.vertexAttributes)
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
      (subpassInfo.subpassIndex) -- subpass index
      Vk.NULL_HANDLE -- pipeline handle (inheritance)
      (-1) -- pipeline index (inheritance)

  pipeline <-
    Vector.head <$> withVkGraphicsPipelines
      (rendererDevice r)
      Vk.NULL_HANDLE
      (Vector.singleton pipelineCreateInfo)

  pure $ Subpass pipeline pipelineLayout

evalSubpass
  :: (MonadUnliftIO m, MonadMask m, MonadResource m)
  => Renderer
  -> Swapchain
  -> Handle Subpass
  -> m Subpass
evalSubpass r swapchain (SubpassHandle d) = do
  (_relKey, pass) <- evalEmpty d $ \subpassInfo -> do
    (RenderPassLayout rp _framebuffers) <-
      evalRenderPassLayout r swapchain subpassInfo.layout
    shaders
      <- mapM (evalShader r) subpassInfo.shaderStages
    allocateAcquire $ acquireSubpass r subpassInfo shaders rp
  pure pass

evalSubpass'
  :: (MonadUnliftIO m, MonadMask m, MonadResource m)
  => Renderer
  -> Swapchain
  -> Handle Subpass
  -> m Subpass
evalSubpass' r swapchain (SubpassHandle d) = do
  (_relKey, pass) <- eval d $ \subpassInfo mOld -> do
    (RenderPassLayout rp _framebuffers) <-
      evalRenderPassLayout r swapchain subpassInfo.layout
    shaders
      <- mapM (evalShader r) subpassInfo.shaderStages
    allocateAcquire $ acquireSubpass r subpassInfo shaders rp
      <* (case mOld of
           Nothing -> pure ()
           Just (oldRel, _) -> release oldRel
         )
  pure pass

acquireRenderPassLayout :: Renderer -> Swapchain -> Acquire RenderPassLayout
acquireRenderPassLayout r swapchain = do
  rp <- withVkRenderPass (rendererDevice r)
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
      withVkFramebuffer (rendererDevice r)
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

evalRenderPassLayout
  :: (MonadMask m, MonadResource m)
  => Renderer
  -> Swapchain
  -> Handle RenderPassLayout
  -> m RenderPassLayout
evalRenderPassLayout r swapchain (RenderPassLayoutHandle d) = do
  fmap snd <$> evalEmpty d $ \_ -> allocateAcquire $
    acquireRenderPassLayout r swapchain

evalRenderPassLayout'
  :: (MonadMask m, MonadResource m)
  => Renderer
  -> Swapchain
  -> Handle RenderPassLayout
  -> m RenderPassLayout
evalRenderPassLayout' r swapchain (RenderPassLayoutHandle d) = do
  fmap snd <$> eval d $ \_ mOld -> do
    allocateAcquire (acquireRenderPassLayout r swapchain)
    <* (case mOld of
          Nothing -> pure ()
          Just (oldRel, _) -> release oldRel
       )


evalBuffer
  :: (MonadUnliftIO m, MonadMask m, MonadResource m)
  => VkContext
  -> Renderer
  -> Handle Buffer
  -> m Buffer
evalBuffer ctx r (BufferHandle d) = do
  evalEmpty d $ \createInfo ->
    with createInfo.dat $ \(sz, bytes :: [x]) -> do
      (_relKey, alloc) <- allocateAcquire $
        withBuffer (vkAllocator ctx) createInfo.usage sz
      withAllocPtr alloc $ \ptr -> do
        liftIO $ putStrLn $ show ptr
        liftIO $ pokeArray (castPtr @() @x ptr) bytes
      _ <- flush (vkAllocator ctx) alloc 0 sz
      case requiresBufferCopy alloc of
        Nothing -> pure ()
        Just (srcBuf, dstBuf) -> do
          liftIO $ STM.atomically $ STM.writeTQueue r.rendererPendingWrites $
            BufferCopyInfo { src = srcBuf
                           , dst = dstBuf
                           , regions = Vector.singleton $ Vk.BufferCopy 0 0 sz
                           , dstStageMask = Vk.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR
                           }
      pure $ Buffer (getAllocBuffer alloc) (getAllocOffset alloc)

evalShader
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadResource m
     )
  => Renderer -> Handle Shader -> m Shader
evalShader r (ShaderHandle d) = do
  evalEmpty d $ \createInfo -> do
    with createInfo.dat $ \shaderCode -> do
      (_relKey, shaderModule) <- allocateAcquire $
        withVkShaderModule (rendererDevice r) $
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

-- mkFrameData
--   :: VkContext
--   -> Swapchain
--   -> [RenderPassInfo]
--   -> Acquire [FrameData]
-- mkFrameData ctx swapchain renderPassInfos = do
--   renderPasses <- forM renderPassInfos $ \renderPassInfo -> do
--     rp <- withVkRenderPass (vkDevice ctx)
--            $ Vk.RenderPassCreateInfo
--                ()
--                Vk.zero
--                -- attachment descriptions
--                ( Vector.singleton $ Vk.AttachmentDescription
--                    Vk.zero
--                    (VkFormat.format $ vkSurfaceFormat swapchain)
--                    Vk.SAMPLE_COUNT_1_BIT            -- Samples
--                    Vk.ATTACHMENT_LOAD_OP_CLEAR      -- Load op, what to do with framebuffer before rendering
--                    Vk.ATTACHMENT_STORE_OP_STORE     -- Store op, store the framebuffer
--                    Vk.ATTACHMENT_LOAD_OP_DONT_CARE  -- Stencil load op
--                    Vk.ATTACHMENT_STORE_OP_DONT_CARE -- Stencil store op
--                    Vk.IMAGE_LAYOUT_UNDEFINED        -- Initial layout
--                    Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR  -- Final layout
--                )
--                -- subpass descriptions
--                ( Vector.singleton $ Vk.SubpassDescription
--                    Vk.zero
--                    Vk.PIPELINE_BIND_POINT_GRAPHICS
--                    Vector.empty -- input attachments
--                    -- color attachments
--                    ( Vector.singleton $ Vk.AttachmentReference
--                        0 -- attachment ix
--                        Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
--                    )
--                    Vector.empty -- resolve attachments
--                    Nothing      -- depth stencil attachments
--                    Vector.empty -- preserve attachments
--                )
--                -- subpass dependencies
--                ( Vector.singleton $ Vk.SubpassDependency
--                    Vk.SUBPASS_EXTERNAL -- src subpass
--                    0 -- dst subpass
--                    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT -- src stage mask
--                    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT -- dst stage mask
--                    Vk.zero -- src access mask
--                    Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT -- dst access mask
--                    Vk.zero -- dependency flags
--                )
--     subpasses <-
--       forM (zip [0..] $ renderPassInfoSubpasses renderPassInfo) $ \(subpassIx, subpassInfo) -> do
--         pipelineLayout <-
--           withVkPipelineLayout (vkDevice ctx)
--             $ Vk.PipelineLayoutCreateInfo
--                 Vk.zero
--                 -- set layouts
--                 (Vector.fromList $ subpassInfoDescriptors subpassInfo)
--                 -- push constant ranges
--                 Vector.empty
--         let
--           pipelineCreateInfo = Vk.GraphicsPipelineCreateInfo () Vk.zero
--             (fromIntegral $ length $ subpassInfoShaderStages subpassInfo)
--             (fmap Vk.SomeStruct . Vector.fromList $ subpassInfoShaderStages subpassInfo)
--             ( Just . Vk.SomeStruct
--               $ Vk.PipelineVertexInputStateCreateInfo
--                   ()
--                   Vk.zero
--                   (Vector.fromList $ subpassInfoVertexBindings subpassInfo)
--                   (Vector.fromList $ subpassInfoVertexAttributes subpassInfo)
--             )
--             ( Just
--               $ Vk.PipelineInputAssemblyStateCreateInfo
--                   Vk.zero
--                   Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST -- Topology
--                   False -- Primitive restart
--             )
--             Nothing -- tesselation
--             ( Just . Vk.SomeStruct
--               $ Vk.PipelineViewportStateCreateInfo
--                   ()
--                   Vk.zero
--                   -- Viewports (dynamic so empty)
--                   1
--                   Vector.empty
--                   -- Scissors (dynamic so empty)
--                   1
--                   Vector.empty
--             )
--             ( Just . Vk.SomeStruct
--              $ Vk.PipelineRasterizationStateCreateInfo
--                  ()
--                  Vk.zero
--                  False -- Depth clamp
--                  False -- Rasterizer discard
--                  Vk.POLYGON_MODE_FILL -- Polygon mode
--                  Vk.CULL_MODE_NONE -- Cull mode
--                  Vk.FRONT_FACE_CLOCKWISE -- Facing
--                  False -- Depth bias
--                  0 -- Depth bias constant factor
--                  0 -- Depth bias clamp
--                  0 -- Depth bias slope factor
--                  1 -- Line width
--             )
--             ( Just . Vk.SomeStruct
--                 $ Vk.PipelineMultisampleStateCreateInfo
--                     ()
--                     Vk.zero
--                     Vk.SAMPLE_COUNT_1_BIT -- Number of samples
--                     False -- Sample shading enable
--                     1 -- Min sample shading
--                     Vector.empty -- Sample mask
--                     False -- Alpha to coverage
--                     False -- Alpha to one
--             )
--             ( Just
--                 $ Vk.PipelineDepthStencilStateCreateInfo
--                     Vk.zero
--                     True -- depth test enable
--                     True -- depth write enable
--                     Vk.COMPARE_OP_LESS -- compare op
--                     False -- bounds test
--                     False -- stencil test enable
--                     Vk.zero -- front stencil
--                     Vk.zero -- back stencil
--                     0 -- min depth
--                     1 -- max depth
--             )
--             ( Just . Vk.SomeStruct
--                 $ Vk.PipelineColorBlendStateCreateInfo
--                     ()
--                     Vk.zero
--                     False -- Logic op enable
--                     Vk.LOGIC_OP_COPY -- logic op
--                     1 -- attachment count
--                     -- attachments
--                     ( Vector.singleton
--                       $ Vk.PipelineColorBlendAttachmentState
--                           False -- blend enable
--                           Vk.zero -- src color blend factor
--                           Vk.zero -- dst color blend factor
--                           Vk.zero -- color blend op
--                           Vk.zero -- src alpha blend factor
--                           Vk.zero -- dst alpha blend factor
--                           Vk.zero -- alpha blend op
--                           -- color write mask
--                           (Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT)
--                     )
--                     (0, 0, 0, 0) -- Blend constants
--             )
--             ( Just
--                 $ Vk.PipelineDynamicStateCreateInfo
--                     Vk.zero
--                     (Vector.fromList [ Vk.DYNAMIC_STATE_VIEWPORT
--                                      , Vk.DYNAMIC_STATE_SCISSOR
--                                      ]
--                     )
--             )
--             pipelineLayout
--             rp
--             subpassIx -- subpass index
--             Vk.NULL_HANDLE -- pipeline handle (inheritance)
--             (-1) -- pipeline index (inheritance)

--         pipeline <-
--           Vector.head <$> withVkGraphicsPipelines
--             (vkDevice ctx)
--             Vk.NULL_HANDLE
--             (Vector.singleton pipelineCreateInfo)

--         pure $ Subpass pipeline pipelineLayout (subpassInfoDraw subpassInfo)

--     pure $ RenderPass rp subpasses

--   frameDatas <-
--     forM (Vector.toList $ vkImageViews swapchain) $ \iv -> do
--       fmap FrameData <$> forM renderPasses $ \rp -> do
--         framebuffer <- withVkFramebuffer (vkDevice ctx)
--           $ Vk.FramebufferCreateInfo
--             ()
--             Vk.zero
--             (renderPass rp) -- render pass
--             (Vector.singleton iv) -- attachments
--             -- dimensions
--             (Extent2D.width $ vkExtent swapchain)
--             (Extent2D.height $ vkExtent swapchain)
--             1 -- layers
--         pure $ (framebuffer, rp)

--   pure frameDatas

-- recordFrameData
--   :: MonadIO m
--   => Vk.CommandBuffer
--   -> Swapchain
--   -> FrameData
--   -> m ()
-- recordFrameData cmdBuffer swapchain frameData = do
--   forM_ (frameRenderPasses frameData) $ \(framebuffer, rp) -> do
--     let
--       clearValues = Vector.fromList [Vk.Color $ Vk.Float32 0 0 0 0]
--       renderStartPos = Vk.Offset2D 0 0
--       renderExtent = vkExtent swapchain
--       renderPassBeginInfo =
--         Vk.RenderPassBeginInfo
--           ()
--           (renderPass rp)
--           framebuffer
--           (Vk.Rect2D renderStartPos renderExtent)
--           clearValues

--     Vk.cmdBeginRenderPass
--       cmdBuffer renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE

--     let
--       viewport = Vk.Viewport
--         0 -- startX
--         0 -- startY
--         (fromIntegral $ Extent2D.width renderExtent) -- width
--         (fromIntegral $ Extent2D.height renderExtent) -- height
--         0 -- min depth
--         1 -- max depth
--       scissor = Vk.Rect2D renderStartPos renderExtent
--     Vk.cmdSetViewport
--       cmdBuffer
--       0
--       (Vector.singleton viewport)
--     Vk.cmdSetScissor
--       cmdBuffer
--       0
--       (Vector.singleton scissor)

--     forM_ (renderPassSubpasses rp) $ \(Subpass pipeline _pipelineLayout draw) -> do
--       Vk.cmdBindPipeline
--         cmdBuffer
--         Vk.PIPELINE_BIND_POINT_GRAPHICS
--         pipeline

--       let
--         (vertexBufs, vertexOffsets) =
--           unzip
--           $ drawVertexBuffers draw
--           <&> \(BufferRef vertexBuf offset) -> (vertexBuf, offset)
--       unless (null vertexBufs) $
--         Vk.cmdBindVertexBuffers
--           cmdBuffer
--           0 -- first binding
--           (Vector.fromList vertexBufs)
--           (Vector.fromList vertexOffsets)
--       forM_ (drawIndexBuffers draw) $ \(BufferRef indexBuf offset, indexType) -> do
--         Vk.cmdBindIndexBuffer
--           cmdBuffer
--           indexBuf
--           offset
--           indexType

--       case drawCall draw of
--         (IndexedDraw (DrawCallIndexed indexCount instanceCount firstIndex vertexOffset firstInstance)) ->
--           Vk.cmdDrawIndexed
--             cmdBuffer indexCount instanceCount firstIndex vertexOffset firstInstance
--         (PrimitiveDraw (DrawCallPrimitive firstVertex firstInstance instanceCount vertexCount)) ->
--           Vk.cmdDraw
--             cmdBuffer vertexCount instanceCount firstVertex firstInstance

--     Vk.cmdEndRenderPass cmdBuffer
