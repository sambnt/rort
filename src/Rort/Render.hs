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
import Rort.Render.Swapchain (Swapchain, vkSurfaceFormat, vkImageViews, vkExtent, throwSwapchainOutOfDate, throwSwapchainSubOptimal)
import Rort.Render.Types ( RenderPassInfo, FrameData, DrawCall(PrimitiveDraw, IndexedDraw), Subpass(Subpass), DrawCallPrimitive(..), RenderPass(..), frameRenderPasses, FrameData(FrameData), SubpassInfo(..), Draw(..), DrawCallIndexed(..), BufferRef(..), RenderPassInfo(..) )
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.Vector as Vector
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import Rort.Vulkan (withVkRenderPass, withVkGraphicsPipelines, withVkFramebuffer, withVkPipelineLayout, withVkShaderModule)
import qualified Vulkan.Extensions.VK_KHR_surface as VkFormat
import qualified Vulkan.CStruct.Extends as Vk
import Data.Bits ((.|.))
import Control.Monad (forM, forM_, unless, foldM_)
import Data.Functor ((<&>), void)
import Rort.Render.FramesInFlight (FrameSync (..))
import Data.Word (Word32, Word64, Word8)
import Control.Monad.IO.Class (MonadIO (..))
import Foreign (nullPtr, Storable, pokeArray, advancePtr, castPtr)
import Data.Acquire (Acquire, with, allocateAcquire)
import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Data.ByteString.Lazy (ByteString, unpack)
import Rort.Render.Pool (Pool, newPool, Handle, addItem, getItem)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO)
import Control.Monad.Cont (ContT(ContT), runContT)

data Renderer = Renderer { shaders :: Pool ShaderCreateInfo
                         , buffers :: Pool BufferCreateInfo
                         }

data BufferCreateInfo
  = BufferCreateInfo { usage :: Vk.BufferUsageFlagBits
                     , dat :: Acquire (Word64, ByteString)
                     }

data ShaderCreateInfo
  = ShaderCreateInfo { shaderStage :: Vk.ShaderStageFlagBits
                     , entryFn     :: ByteString
                     , dat         :: Acquire ByteString
                     }

data ShaderCreated
  = ShaderCreated { pipelineShaderStage :: Vk.PipelineShaderStageCreateInfo '[]
                  }

create :: IO Renderer
create = STM.atomically $ Renderer <$> newPool 64 <*> newPool 64

shader
  :: Renderer
  -> Vk.ShaderStageFlagBits
  -> ByteString
  -> Acquire ByteString
  -> IO (Handle ShaderCreateInfo)
shader r stage entry code = do
  STM.atomically $ addItem r.shaders $ ShaderCreateInfo stage entry code

loadShader
  :: (MonadResource m, MonadUnliftIO m)
  => VkContext
  -> Renderer
  -> Handle ShaderCreateInfo
  -> m ShaderCreated
loadShader ctx r h = do
  createInfo <-
    liftIO $ STM.atomically $ getItem r.shaders h
  shaderModule <- with createInfo.dat $ \shaderCode -> do
    fmap snd $ allocateAcquire $
      withVkShaderModule (vkDevice ctx) $
        Vk.ShaderModuleCreateInfo () Vk.zero shaderCode
  pure $
    ShaderCreated
      $ Vk.PipelineShaderStageCreateInfo
         ()
         Vk.zero
         createInfo.shaderStage
         shaderModule
         createInfo.entryFn
         Nothing

data BufferUsage = BufferVertex | BufferIndex
data Buffer

vertexBuffer
  :: Renderer
  -> Acquire (Word64, ByteString)
  -> IO (Handle BufferCreateInfo)
vertexBuffer r bufferDat = do
  STM.atomically $ addItem r.buffers
    $ BufferCreateInfo Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT bufferDat

loadVertexBuffers
  :: (MonadResource m, MonadUnliftIO m)
  => VkContext
  -> Renderer
  -> [Handle BufferCreateInfo]
  -> m ()
loadVertexBuffers ctx r hs = do
  createInfos <- forM hs $ \h -> liftIO . STM.atomically $ do
    (h,) <$> getItem r.buffers h
  let
    acquires :: [Acquire (Word64, ByteString)]
    acquires = createInfos <&> (\c -> c.bufferDat)
    allAcquires :: Acquire [(Word64, ByteString)]
    allAcquires = sequence acquires

  vkBuf <- with allAcquires $ \allData -> do
    let totalSize = sum (fst <$> allData)
    (vkBuf, ptr) <- allocateAcquire $ withBuffer (vkAllocator ctx) totalSize
    foldM_
      (\ptr' (sz, bytes) -> liftIO $ do
        -- Write data to Vulkan buffer
        pokeArray ptr' (unpack bytes)
        -- Write buffer and offset back to handle
        pure $ advancePtr ptr' (fromIntegral sz)
      )
      (castPtr @() @Word8 ptr) allData
    pure vkBuf

  pure vkBuf

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

mkFrameData
  :: VkContext
  -> Swapchain
  -> [RenderPassInfo]
  -> Acquire [FrameData]
mkFrameData ctx swapchain renderPassInfos = do
  renderPasses <- forM renderPassInfos $ \renderPassInfo -> do
    rp <- withVkRenderPass (vkDevice ctx)
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
    subpasses <-
      forM (zip [0..] $ renderPassInfoSubpasses renderPassInfo) $ \(subpassIx, subpassInfo) -> do
        pipelineLayout <-
          withVkPipelineLayout (vkDevice ctx)
            $ Vk.PipelineLayoutCreateInfo
                Vk.zero
                -- set layouts
                (Vector.fromList $ subpassInfoDescriptors subpassInfo)
                -- push constant ranges
                Vector.empty
        let
          pipelineCreateInfo = Vk.GraphicsPipelineCreateInfo () Vk.zero
            (fromIntegral $ length $ subpassInfoShaderStages subpassInfo)
            (fmap Vk.SomeStruct . Vector.fromList $ subpassInfoShaderStages subpassInfo)
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
            subpassIx -- subpass index
            Vk.NULL_HANDLE -- pipeline handle (inheritance)
            (-1) -- pipeline index (inheritance)

        pipeline <-
          Vector.head <$> withVkGraphicsPipelines
            (vkDevice ctx)
            Vk.NULL_HANDLE
            (Vector.singleton pipelineCreateInfo)

        pure $ Subpass pipeline pipelineLayout (subpassInfoDraw subpassInfo)

    pure $ RenderPass rp subpasses

  frameDatas <-
    forM (Vector.toList $ vkImageViews swapchain) $ \iv -> do
      fmap FrameData <$> forM renderPasses $ \rp -> do
        framebuffer <- withVkFramebuffer (vkDevice ctx)
          $ Vk.FramebufferCreateInfo
            ()
            Vk.zero
            (renderPass rp) -- render pass
            (Vector.singleton iv) -- attachments
            -- dimensions
            (Extent2D.width $ vkExtent swapchain)
            (Extent2D.height $ vkExtent swapchain)
            1 -- layers
        pure $ (framebuffer, rp)

  pure frameDatas

recordFrameData
  :: MonadIO m
  => Vk.CommandBuffer
  -> Swapchain
  -> FrameData
  -> m ()
recordFrameData cmdBuffer swapchain frameData = do
  forM_ (frameRenderPasses frameData) $ \(framebuffer, rp) -> do
    let
      clearValues = Vector.fromList [Vk.Color $ Vk.Float32 0 0 0 0]
      renderStartPos = Vk.Offset2D 0 0
      renderExtent = vkExtent swapchain
      renderPassBeginInfo =
        Vk.RenderPassBeginInfo
          ()
          (renderPass rp)
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

    forM_ (renderPassSubpasses rp) $ \(Subpass pipeline _pipelineLayout draw) -> do
      Vk.cmdBindPipeline
        cmdBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        pipeline

      let
        (vertexBufs, vertexOffsets) =
          unzip
          $ drawVertexBuffers draw
          <&> \(BufferRef vertexBuf offset) -> (vertexBuf, offset)
      unless (null vertexBufs) $
        Vk.cmdBindVertexBuffers
          cmdBuffer
          0 -- first binding
          (Vector.fromList vertexBufs)
          (Vector.fromList vertexOffsets)
      forM_ (drawIndexBuffers draw) $ \(BufferRef indexBuf offset, indexType) -> do
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
