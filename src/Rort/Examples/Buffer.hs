{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rort.Examples.Buffer where

import Rort.Window (withWindow, getRequiredExtensions, getWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..), graphicsQueueFamilies)
import Control.Monad.Trans.Resource (runResourceT, MonadResource, ResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Rort.Render.Swapchain (withSwapchain, vkSurfaceFormat, vkImageViews, vkExtent, fsFenceInFlight, withNextFrameInFlight, fsSemaphoreImageAvailable, vkSwapchain, throwSwapchainOutOfDate, fsSemaphoreRenderFinished, throwSwapchainSubOptimal, Swapchain, retryOnSwapchainOutOfDate)
import Rort.Vulkan (withVkShaderModule, withVkPipelineLayout, withVkRenderPass, withVkGraphicsPipelines, withVkFramebuffer, withVkCommandBuffers, withVkCommandPool, withVkBuffer, withVkBufferMemory, copyBuffer)
import qualified Vulkan.Extensions.VK_KHR_surface as VkFormat
import qualified Vulkan.Zero as Vk
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import qualified Rort.Util.Resource as Resource
import qualified Vulkan.CStruct.Extends as Vk
import Data.Bits ((.|.))
import Control.Monad (forM, forM_, when, unless)
import Data.Functor (void, (<&>))
import Foreign (nullPtr, sizeOf, Word16, castPtr, pokeArray, advancePtr)
import Rort.Window.Types (WindowEvent(..))
import Rort.Util.Resource (Resource)
import Data.Word (Word64)

main :: IO ()
main = do
  let
    width = 800
    height = 600

  withWindow width height "Example: Buffer" $ \win -> do
    windowExts <- getRequiredExtensions win

    runResourceT $ do
      let
        cfg = VkSettings { requiredExtensions =
                             windowExts <> Vector.fromList []
                         , requiredValidationLayers =
                             Vector.fromList [ "VK_LAYER_KHRONOS_validation" ]
                         , applicationInfo =
                             Vk.ApplicationInfo
                               (Just "Example: Buffer")  -- application name
                               (Vk.MAKE_API_VERSION 1 0 0) -- application version
                               (Just "No engine")          -- engine name
                               (Vk.MAKE_API_VERSION 1 0 0) -- engine version
                               (Vk.MAKE_API_VERSION 1 0 0) -- Vulkan API version (patch version ignored)
                         }

      ctx <- withVkContext cfg win

      vertShaderCode <- liftIO $ BS.readFile "data/vertexBuffers.vert.spv"
      fragShaderCode <- liftIO $ BS.readFile "data/tri.frag.spv"

      let numFramesInFlight = 2
      framebufferSize <- liftIO $ vkGetFramebufferSize ctx
      initialSwapchain <-
        withSwapchain ctx numFramesInFlight framebufferSize Nothing

      vertShader <-
        withVkShaderModule (vkDevice ctx)
          $ Vk.ShaderModuleCreateInfo () Vk.zero vertShaderCode
      fragShader <-
        withVkShaderModule (vkDevice ctx)
          $ Vk.ShaderModuleCreateInfo () Vk.zero fragShaderCode

      let
        pipelineShaderStages =
          [ Vk.PipelineShaderStageCreateInfo
              ()
              Vk.zero
              Vk.SHADER_STAGE_VERTEX_BIT
              (Resource.get vertShader)
              "main"
              Nothing
          , Vk.PipelineShaderStageCreateInfo
              ()
              Vk.zero
              Vk.SHADER_STAGE_FRAGMENT_BIT
              (Resource.get fragShader)
              "main"
              Nothing
          ]

      pipelineLayout <-
        withVkPipelineLayout (vkDevice ctx)
          $ Vk.PipelineLayoutCreateInfo
              Vk.zero
              Vector.empty -- set layouts
              Vector.empty -- push constant ranges

      cmdPool <-
        withVkCommandPool
          (vkDevice ctx)
          (NE.head . graphicsQueueFamilies $ vkQueueFamilies ctx)

      let
        vertices :: [Float]
        vertices = [ -0.5, -0.5, 1, 0, 0
                   ,  0.5, -0.5, 0, 1, 0
                   ,  0.5,  0.5, 0, 0, 1
                   , -0.5,  0.5, 1, 1, 1
                   ]
        vertexBufferSize = fromIntegral $
          sizeOf (undefined :: Float) * length vertices

        indices :: [Word16]
        indices = [ 2, 1, 0, 0, 3, 2 ]

        indexBufferSize = fromIntegral $
          sizeOf (undefined :: Word16) * length indices

      stagingBuffer <- withVkBuffer (vkDevice ctx)
        $ Vk.BufferCreateInfo
            ()
            Vk.zero
            (vertexBufferSize + indexBufferSize)
            (Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT .|. Vk.BUFFER_USAGE_INDEX_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_SRC_BIT)
            Vk.SHARING_MODE_EXCLUSIVE
            Vector.empty -- queue family indices, ignored.
      stagingBufferMem <-
        withVkBufferMemory
          (vkPhysicalDevice ctx)
          (vkDevice ctx)
          (Resource.get stagingBuffer)
          (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
      liftIO $ Vk.bindBufferMemory
        (vkDevice ctx)
        (Resource.get stagingBuffer)
        (Resource.get stagingBufferMem)
        0 -- offset
      ptr <- liftIO $ Vk.mapMemory
        (vkDevice ctx)
        (Resource.get stagingBufferMem)
        0 -- offset
        (vertexBufferSize + indexBufferSize)
        Vk.zero
      let vertexPtr = castPtr @() @Float ptr
      liftIO $ pokeArray vertexPtr vertices
      let indexPtr = castPtr @Float @Word16 (advancePtr vertexPtr $ length vertices)
      liftIO $ pokeArray indexPtr indices

      deviceBuffer <- withVkBuffer (vkDevice ctx)
        $ Vk.BufferCreateInfo
            ()
            Vk.zero
            (vertexBufferSize + indexBufferSize)
            (Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT .|. Vk.BUFFER_USAGE_INDEX_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT)
            Vk.SHARING_MODE_EXCLUSIVE
            Vector.empty -- queue family indices, ignored.
      deviceBufferMem <-
        withVkBufferMemory
          (vkPhysicalDevice ctx)
          (vkDevice ctx)
          (Resource.get deviceBuffer)
          Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      liftIO $ Vk.bindBufferMemory
        (vkDevice ctx)
        (Resource.get deviceBuffer)
        (Resource.get deviceBufferMem)
        0 -- offset

      liftIO $ copyBuffer
        (vkDevice ctx)
        (Resource.get cmdPool)
        (vkGraphicsQueue ctx)
        (vertexBufferSize + indexBufferSize)
        (Resource.get stagingBuffer)
        (Resource.get deviceBuffer)

      retryOnSwapchainOutOfDate ctx initialSwapchain $ \swapchain -> do
        -- BEGIN swapchain-dependent
        let
          subpassInfo =
            SubpassInfo { subpassInfoShaderStages = pipelineShaderStages
                        , subpassInfoPipelineLayout = Resource.get pipelineLayout
                        , subpassInfoVertexBindings =
                            [ Vk.VertexInputBindingDescription
                                0 -- first vertex buffer bound
                                (fromIntegral $ sizeOf (undefined :: Float) * 5)
                                Vk.VERTEX_INPUT_RATE_VERTEX
                            ]
                        , subpassInfoVertexAttributes =
                          [ Vk.VertexInputAttributeDescription
                              0 -- location (pos)
                              0 -- binding
                              Vk.FORMAT_R32G32_SFLOAT
                              0 -- offset
                          , Vk.VertexInputAttributeDescription
                              1 -- location (color)
                              0 -- binding
                              Vk.FORMAT_R32G32B32_SFLOAT
                              (fromIntegral $ sizeOf (undefined :: Float) * 2)
                          ]
                        , subpassInfoDraw =
                            Draw { drawCall = ()
                                 , drawVertexBuffers =
                                    [ BufferRef (Resource.get deviceBuffer) 0 ]
                                 , drawIndexBuffers =
                                    [ ( BufferRef (Resource.get deviceBuffer) vertexBufferSize
                                      , Vk.INDEX_TYPE_UINT16
                                      )
                                    ]
                                 }
                        }
          renderPassInfo = RenderPassInfo [subpassInfo]
        frameDatas <-
          mkFrameData
            ctx
            swapchain
            [renderPassInfo]
        -- END swapchain-dependent

        -- rendering a frame
        let
          -- loop :: ResourceT IO ()
          loop = do
            withNextFrameInFlight swapchain $ \fs -> do
              void $ Vk.waitForFences
                (vkDevice ctx)
                (Vector.singleton $ fsFenceInFlight fs)
                True
                maxBound

              (_, imageIndex) <- liftIO $ throwSwapchainOutOfDate $
                Vk.acquireNextImageKHR
                  (vkDevice ctx)
                  (vkSwapchain swapchain)
                  maxBound
                  (fsSemaphoreImageAvailable fs)
                  Vk.NULL_HANDLE

              -- Only reset fences if we know we are submitting work, otherwise we
              -- might deadlock later while waiting for the fence to signal.
              liftIO $ Vk.resetFences
                (vkDevice ctx)
                (Vector.singleton $ fsFenceInFlight fs)

              cmdBuffers <-
                withVkCommandBuffers
                  (vkDevice ctx)
                  $ Vk.CommandBufferAllocateInfo
                      (Resource.get cmdPool)
                      -- Primary = can be submitted to queue for execution, can't be
                      -- called by other command buffers.
                      Vk.COMMAND_BUFFER_LEVEL_PRIMARY
                      1 -- count

              let cmdBuffer = Vector.head $ Resource.get cmdBuffers
              Vk.beginCommandBuffer cmdBuffer
                $ Vk.CommandBufferBeginInfo
                    ()
                    Vk.zero
                    Nothing -- Inheritance info

              let
                frameData = Resource.get frameDatas !! fromIntegral imageIndex
              forM_ (frameRenderPasses frameData) $ \(framebuffer, rp) -> do
                let
                  clearValues = Vector.fromList [Vk.Color $ Vk.Float32 (254/255) (243/255) (215/255) 0]
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

                forM_ (renderPassSubpasses rp) $ \(Subpass pipeline draw) -> do
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

                  let
                    indexCount = 6
                    instanceCount = 1
                    firstIndex = 0
                    vertexOffset = 0
                    firstInstance = 0
                  Vk.cmdDrawIndexed
                    cmdBuffer indexCount instanceCount firstIndex vertexOffset firstInstance

                Vk.cmdEndRenderPass cmdBuffer

              Vk.endCommandBuffer cmdBuffer

              let
                submitInfo =
                  Vk.SubmitInfo
                    ()
                    (Vector.singleton $ fsSemaphoreImageAvailable fs)
                    (Vector.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                    (Vector.singleton $ Vk.commandBufferHandle cmdBuffer)
                    (Vector.singleton $ fsSemaphoreRenderFinished fs)
              liftIO $ Vk.queueSubmit
                (vkGraphicsQueue ctx)
                (Vector.singleton $ Vk.SomeStruct submitInfo)
                (fsFenceInFlight fs)

              let
                presentInfo = Vk.PresentInfoKHR
                  ()
                  (Vector.singleton $ fsSemaphoreRenderFinished fs)
                  (Vector.singleton $ vkSwapchain swapchain)
                  (Vector.singleton imageIndex)
                  nullPtr
              liftIO $ void
                $ (throwSwapchainSubOptimal =<<)
                $ throwSwapchainOutOfDate
                $ Vk.queuePresentKHR (vkPresentationQueue ctx) presentInfo

            mEv <- liftIO $ getWindowEvent win
            shouldContinue <- liftIO $ case mEv of
              Just (WindowError err) -> do
                putStrLn $ "Error " <> show err
                closeWindow win
                pure False
              Just WindowClose -> do
                putStrLn "Window closing..."
                closeWindow win
                pure False
              Just (WindowResize x y) -> do
                putStrLn $ "Window resizing (" <> show x <> ", " <> show y <> ")"
                pure True
              Nothing -> pure True
            when shouldContinue loop
        loop

-- input
data BufferRef = BufferRef { bufRefBuffer :: Vk.Buffer
                           , bufRefOffset :: Word64
                           }

data Draw
  = Draw { drawCall :: ()
         , drawVertexBuffers :: [BufferRef]
         , drawIndexBuffers  :: [(BufferRef, Vk.IndexType)]
         }

data SubpassInfo
  = SubpassInfo { subpassInfoShaderStages     :: [Vk.PipelineShaderStageCreateInfo '[]]
                , subpassInfoPipelineLayout   :: Vk.PipelineLayout
                , subpassInfoVertexBindings   :: [Vk.VertexInputBindingDescription]
                , subpassInfoVertexAttributes :: [Vk.VertexInputAttributeDescription]
                , subpassInfoDraw             :: Draw
                }

data RenderPassInfo
  = RenderPassInfo { renderPassInfoSubpasses :: [SubpassInfo]
                   }

-- output
data Subpass
  = Subpass { subpassPipeline :: Vk.Pipeline
            , subpassDraw     :: Draw
            }

data RenderPass
  = RenderPass { renderPass :: Vk.RenderPass
               , renderPassSubpasses :: [Subpass]
               }

data FrameData
  = FrameData { frameRenderPasses :: [(Vk.Framebuffer, RenderPass)]
              }


mkFrameData
  :: MonadResource m
  => VkContext
  -> Swapchain
  -> [RenderPassInfo]
  -> m (Resource [FrameData])
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
    let
      pipelineCreateInfos =
        (zip [0..] $ renderPassInfoSubpasses renderPassInfo) <&> \(subpassIx, subpassInfo) ->
          Vk.GraphicsPipelineCreateInfo () Vk.zero
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
            (subpassInfoPipelineLayout subpassInfo)
            (Resource.get rp)
            subpassIx -- subpass index
            Vk.NULL_HANDLE -- pipeline handle (inheritance)
            (-1) -- pipeline index (inheritance)

    pipelines <-
      withVkGraphicsPipelines
        (vkDevice ctx)
        Vk.NULL_HANDLE
        (Vector.fromList pipelineCreateInfos)

    let
      subpasses = do
        ps <- pipelines
        pure $ zip
          (renderPassInfoSubpasses renderPassInfo)
          (Vector.toList ps)
          <&> \(subpassInfo, pipeline) ->
            Subpass pipeline (subpassInfoDraw subpassInfo)

    pure $ RenderPass <$> rp <*> subpasses

  frameDatas <-
    forM (Vector.toList $ vkImageViews swapchain) $ \iv -> do
      fmap (fmap FrameData . sequence) <$> forM renderPasses $ \rp -> do
        framebuffer <- withVkFramebuffer (vkDevice ctx)
          $ Vk.FramebufferCreateInfo
            ()
            Vk.zero
            (renderPass $ Resource.get rp) -- render pass
            (Vector.singleton iv) -- attachments
            -- dimensions
            (Extent2D.width $ vkExtent swapchain)
            (Extent2D.height $ vkExtent swapchain)
            1 -- layers
        pure $ (,) <$> framebuffer <*> rp

  pure $ sequence frameDatas
