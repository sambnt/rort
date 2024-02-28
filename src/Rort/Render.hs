{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Rort.Render where

import Rort.Vulkan.Context ( VkContext, VkContext(..) )
import Rort.Render.Swapchain (vkSurfaceFormat, vkImageViews, vkExtent, throwSwapchainOutOfDate, throwSwapchainSubOptimal, Swapchain, withSwapchain)
import Rort.Render.Types ( RenderPassInfo, RenderPass(..), SubpassInfo(..), RenderPassInfo(..), Handle(..), Buffer(Buffer), Shader(Shader), BufferInfo(..), ShaderInfo(..), Subpass(..), pipelineShaderStage)
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.Vector as Vector
import Rort.Vulkan (withVkShaderModule, withVkFramebuffer, withVkRenderPass, withVkGraphicsPipelines, withVkPipelineLayout)
import qualified Vulkan.CStruct.Extends as Vk
import Data.Functor (void)
import Rort.Render.FramesInFlight (FrameSync (..), FramesInFlight, NumFramesInFlight, withFramesInFlight)
import Data.Word (Word32, Word64, Word8)
import Control.Monad.IO.Class (MonadIO (..))
import Foreign (nullPtr, pokeArray, castPtr)
import Data.Acquire (Acquire, with, allocateAcquire)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
-- import Rort.Render.Pool (Pool, newPool, Handle, addItem, getItem)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO, ReleaseKey)
import Rort.Util.Defer (defer, evalM, unsafeEval)
import Rort.Allocator (withBuffer, flush, withAllocPtr, getAllocBuffer, getAllocOffset)
import Control.Concurrent.STM (TMVar, newTMVarIO, TQueue, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import qualified Vulkan.Extensions.VK_KHR_surface as VkFormat
import Control.Monad (forM)
import Data.Bits ((.|.))

data Renderer
  = Renderer { rendererSwapchain      :: TMVar (ReleaseKey, Swapchain)
             , rendererFramesInFlight :: FramesInFlight
             , rendererPasses         :: TQueue (Handle RenderPass)
             , rendererDevice         :: Vk.Device
             }

create :: MonadResource m => VkContext -> NumFramesInFlight -> m Renderer
create ctx numFramesInFlight = do
 framesInFlight <-
    withFramesInFlight (vkDevice ctx) (vkQueueFamilies ctx) numFramesInFlight

 framebufferSize <-
   liftIO $ vkGetFramebufferSize ctx
 swapchain <-
   allocateAcquire $ do
     withSwapchain ctx framebufferSize Nothing

 swapchainVar <- liftIO $ newTMVarIO swapchain

 swapchainDependentResources <- liftIO newTQueueIO

 pure $ Renderer
   swapchainVar
   framesInFlight
   swapchainDependentResources
   (vkDevice ctx)

step ctx r = undefined

shader
  :: MonadIO m
  => Renderer
  -> Vk.ShaderStageFlagBits
  -> BS.ByteString
  -> Acquire BSL.ByteString
  -> m (Handle Shader)
shader r stage entry code = do
  ShaderHandle <$> defer (ShaderInfo stage entry code)

loadShader
  :: (MonadResource m, MonadUnliftIO m)
  => VkContext
  -> Handle Shader
  -> m Shader
loadShader ctx (ShaderHandle h) = do
  evalM h $ \createInfo -> do
    shaderModule <- with createInfo.dat $ \shaderCode -> do
      fmap snd $ allocateAcquire $
        withVkShaderModule (vkDevice ctx) $
          Vk.ShaderModuleCreateInfo () Vk.zero (BSL.toStrict shaderCode)
    pure $
      Shader
        $ Vk.PipelineShaderStageCreateInfo
           ()
           Vk.zero
           createInfo.shaderStage
           shaderModule
           createInfo.entryFn
           Nothing

vertexBuffer
  :: MonadIO m
  => Renderer
  -> Acquire (Word64, BSL.ByteString)
  -> m (Handle Buffer)
vertexBuffer r bufferDat = do
  BufferHandle
    <$> defer (BufferInfo Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT bufferDat)

indexBuffer
  :: MonadIO m
  => Renderer
  -> Acquire (Word64, BSL.ByteString)
  -> m (Handle Buffer)
indexBuffer r bufferDat = do
  BufferHandle
    <$> defer (BufferInfo Vk.BUFFER_USAGE_INDEX_BUFFER_BIT bufferDat)

loadBuffer
  :: ( MonadUnliftIO m
     , MonadResource m
     )
  => VkContext
  -> Handle Buffer
  -> m Buffer
loadBuffer ctx (BufferHandle h) =
  evalM h $ \createInfo -> do
    with createInfo.dat $ \(sz, bytes) -> do
      (_relKey, alloc) <- allocateAcquire $
        withBuffer (vkAllocator ctx) createInfo.usage sz
      withAllocPtr (vkAllocator ctx) alloc $ \ptr ->
        liftIO $ pokeArray (castPtr @() @Word8 ptr) (BSL.unpack bytes)
      -- TODO: Flush later on? Before first use?
      flush alloc
      -- liftIO . STM.atomically $ setItem r.buffers h vkBuf
      pure $ Buffer (getAllocBuffer alloc) (getAllocOffset alloc)

subpass
  :: (MonadResource m, MonadUnliftIO m)
  => Renderer
  -> SubpassInfo
  -> m (Handle Subpass)
subpass r sInfo = do
  h <- defer sInfo
  evalM h $ \subpassInfo -> allocateAcquire $ do
    (_relKey, RenderPass rp _framebuffers) <-
      liftIO $ unsafeEval ((\(RenderPassHandle rh) -> rh) $ subpassInfoRenderPassLayout subpassInfo)
    pipelineLayout <-
      withVkPipelineLayout (rendererDevice r)
        $ Vk.PipelineLayoutCreateInfo
            Vk.zero
            -- set layouts
            (Vector.fromList $ subpassInfoDescriptors subpassInfo)
            -- push constant ranges
            Vector.empty
    shaderStages <- liftIO $ mapM (\(ShaderHandle s) -> unsafeEval s) $ subpassInfoShaderStages subpassInfo
    let
      pipelineCreateInfo = Vk.GraphicsPipelineCreateInfo () Vk.zero
        (fromIntegral $ length $ subpassInfoShaderStages subpassInfo)
        (fmap Vk.SomeStruct . Vector.fromList $ fmap pipelineShaderStage shaderStages)
        ( Just . Vk.SomeStruct
          $ Vk.PipelineVertexInputStateCreateInfo
              ()
              Vk.zero
              (Vector.fromList $ subpassInfoVertexBindings subpassInfo)
              (Vector.fromList $ subpassInfoVertexAttributes subpassInfo)
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
        (subpassInfoIx subpassInfo) -- subpass index
        Vk.NULL_HANDLE -- pipeline handle (inheritance)
        (-1) -- pipeline index (inheritance)

    pipeline <-
      Vector.head <$> withVkGraphicsPipelines
        (rendererDevice r)
        Vk.NULL_HANDLE
        (Vector.singleton pipelineCreateInfo)

    pure $ Subpass pipeline pipelineLayout
  pure (SubpassHandle h)

  -- TODO create pipeline handles here and on swapchain re-creation

renderPass
  :: (MonadUnliftIO m, MonadResource m)
  => Renderer
  -> RenderPassInfo
  -> m (Handle RenderPass)
renderPass r renderPassInfo = do
  h <- defer renderPassInfo
  liftIO $ STM.atomically $ STM.writeTQueue (rendererPasses r) (RenderPassHandle h)
  (_, swapchain) <-
    liftIO $ STM.atomically $ STM.readTMVar (rendererSwapchain r)
  evalM h $ \_renderPassInfo -> allocateAcquire $ do
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

    pure $ RenderPass rp framebuffers
  pure (RenderPassHandle h)

  -- TODO:
  --   1. Record render pass in tqueue
  --   2. Eval render pass using current swapchain here
  --   3. Re-eval render pass on every new swapchain

-- withFrame ctx r f = do
--   -- step r $ \swapchain -> do
--     (_, cmdPool) <- allocateAcquire $
--       withVkCommandPool (vkDevice ctx) (vkGraphicsQueueIx ctx)
--     (_, cmdBuffers) <-
--       allocateAcquire $ withVkCommandBuffers
--         (vkDevice ctx)
--         $ Vk.CommandBufferAllocateInfo
--             cmdPool
--             -- Primary = can be submitted to queue for execution, can't be
--             -- called by other command buffers.
--             Vk.COMMAND_BUFFER_LEVEL_PRIMARY
--             1 -- count

--     let cmdBuffer = Vector.head cmdBuffers
--     Vk.beginCommandBuffer cmdBuffer
--       $ Vk.CommandBufferBeginInfo
--           ()
--           Vk.zero
--           Nothing -- Inheritance info

--     let draws = f swapchain
--     forM draws $ \draw -> do
--       let
--         subpass = unsafeEval (draw.subpass)
--         renderPass = unsafeEval (undefined subpass)
--       Vk.cmdBeginRenderPass
--         cmdBuffer (_toRenderPassBeginInfo renderPass) Vk.SUBPASS_CONTENTS_INLINE

--       let
--         viewport = undefined subpass
--         scissor = undefined subpass

--       Vk.cmdSetViewport cmdBuffer 0 (Vector.singleton viewport)
--       Vk.cmdSetScissor cmdBuffer 0 (Vector.singleton scissor)

--       Vk.cmdBindPipeline
--         cmdBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS subpass.pipeline

--       unless (null draw.vertexBuffers) $
--         Vk.cmdBindVertexBuffers
--           cmdBuffer
--           0 -- first binding
--           (Vector.fromList $ unsafeEval <$> draw.vertexBuffers)
--           Vector.empty -- offsets
--       case draw.indexBuffer of
--         Nothing -> pure ()
--         Just indexBuf ->
--           Vk.cmdBindIndexBuffer
--             cmdBuffer
--             indexBuf
--             (_offset)
--             (_indexType)

--       Vk.cmdEndRenderPass cmdBuffer
--       Vk.endCommandBuffer cmdBuffer


-- Draw renderPass viewport scissor subpass

-- loadVertexBuffers
--   :: ( MonadUnliftIO m
--      , MonadResource m
--      )
--   => VkContext
--   -> [Handle Buffer]
--   -> m ()
-- loadVertexBuffers ctx hs = do
--   let
--     xs :: [(Deferred BufferCreateInfo Buffer, BufferCreateInfo)]
--     xs = fmap (\(BufferHandle h) -> (h, getSeed h)) hs

--     allAcquires :: Acquire [(Deferred BufferCreateInfo Buffer, (Word64, BSL.ByteString))]
--     allAcquires = mapM (\(h, createInfo) -> do
--                            d <- createInfo.bufferDat
--                            pure (h, d)
--                        ) xs
--   with allAcquires $ \allData -> do
--     let totalSize = sum (fst . snd <$> allData)
--     (_relKey, (vkBuf, ptr)) <-
--       allocateAcquire $ withBuffer (vkAllocator ctx) totalSize
--     foldM_
--       (\(handles, curOffset, ptr') (h, (sz, bytes)) -> liftIO $ do
--         -- Write data to Vulkan buffer
--         pokeArray ptr' (BSL.unpack bytes)
--         _ <- eval h $ const (Buffer vkBuf curOffset)
--         -- Write buffer and offset back to handle
--         pure (h:handles, curOffset + sz, advancePtr ptr' (fromIntegral sz))
--       )
--       ([], 0, castPtr @() @Word8 ptr) allData
--     flush vkBuf

-- TODO: allocator - withBuffer
-- TODO: allocator - flush
-- TODO: hot/cold resources

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
