{-# LANGUAGE ScopedTypeVariables #-}

module Rort.Examples.Triangle where

import Rort.Window (withWindow, getRequiredExtensions)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..), graphicsQueueFamilies)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Rort.Render.Swapchain (withSwapchain, vkSurfaceFormat, vkImageViews, vkExtent, fsFenceInFlight, withNextFrameInFlight, fsSemaphoreImageAvailable, vkSwapchain, throwSwapchainOutOfDate, fsSemaphoreRenderFinished, throwSwapchainSubOptimal)
import Rort.Vulkan (withVkShaderModule, withVkPipelineLayout, withVkRenderPass, withVkGraphicsPipelines, withVkFramebuffer, withVkCommandBuffers, withVkCommandPool)
import qualified Vulkan.Extensions.VK_KHR_surface as VkFormat
import qualified Vulkan.Zero as Vk
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import qualified Rort.Util.Resource as Resource
import qualified Vulkan.CStruct.Extends as Vk
import Data.Bits ((.|.))
import Control.Monad (forM, forM_)
import Data.Functor (void)
import Foreign (nullPtr)

main = do
  let
    width = 800
    height = 600

  withWindow width height "Example: Triangle" $ \win -> do
    windowExts <- getRequiredExtensions win


    runResourceT $ do
      let
        cfg = VkSettings { requiredExtensions =
                             windowExts <> Vector.fromList []
                         , requiredValidationLayers =
                             Vector.fromList [ "VK_LAYER_KHRONOS_validation" ]
                         , applicationInfo =
                             Vk.ApplicationInfo
                               (Just "Example: Triangle")  -- application name
                               (Vk.MAKE_API_VERSION 1 0 0) -- application version
                               (Just "No engine")          -- engine name
                               (Vk.MAKE_API_VERSION 1 0 0) -- engine version
                               (Vk.MAKE_API_VERSION 1 0 0) -- Vulkan API version (patch version ignored)
                         }

      ctx <- withVkContext cfg win

      vertShaderCode <- liftIO $ BS.readFile "data/tri.vert.spv"
      fragShaderCode <- liftIO $ BS.readFile "data/tri.frag.spv"

      let numFramesInFlight = 2
      framebufferSize <- liftIO $ vkGetFramebufferSize ctx
      swapchain <- withSwapchain ctx numFramesInFlight framebufferSize Nothing

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


      -- BEGIN swapchain-dependent
      renderPass
        <- withVkRenderPass (vkDevice ctx)
             $ Vk.RenderPassCreateInfo
                 ()
                 Vk.zero
                 -- attachment descriptions
                 ( Vector.singleton $ Vk.AttachmentDescription
                     Vk.zero
                     (VkFormat.format $ vkSurfaceFormat $ Resource.get swapchain)
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

      pipelines <-
        withVkGraphicsPipelines
          (vkDevice ctx)
          Vk.NULL_HANDLE -- pipeline cache
          ( Vector.singleton
            $ Vk.GraphicsPipelineCreateInfo () Vk.zero
                (fromIntegral $ length pipelineShaderStages)
                (Vk.SomeStruct <$> Vector.fromList pipelineShaderStages)
                ( Just . Vk.SomeStruct
                  $ Vk.PipelineVertexInputStateCreateInfo
                      ()
                      Vk.zero
                      Vector.empty
                      Vector.empty
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
                (Resource.get pipelineLayout)
                (Resource.get renderPass)
                0 -- subpass index
                Vk.NULL_HANDLE -- pipeline handle (inheritance)
                (-1) -- pipeline index (inheritance)
          )

      let renderPasses = [(renderPass, pipelines)]
      framebuffers <-
        forM (vkImageViews $ Resource.get swapchain) $ \iv -> do
          forM renderPasses $ \(rp, ps) -> do
            framebuffer <- withVkFramebuffer (vkDevice ctx)
              $ Vk.FramebufferCreateInfo
                  ()
                  Vk.zero
                  (Resource.get rp) -- render pass
                  (Vector.singleton iv) -- attachments
                  -- dimensions
                  (Extent2D.width $ vkExtent $ Resource.get swapchain)
                  (Extent2D.height $ vkExtent $ Resource.get swapchain)
                  1 -- layers
            pure (rp, ps, framebuffer)
      -- END swapchain-dependent

      cmdPool <-
        withVkCommandPool
          (vkDevice ctx)
          (NE.head . graphicsQueueFamilies $ vkQueueFamilies ctx)

      -- rendering a frame
      let
        loop = do
          withNextFrameInFlight (Resource.get swapchain) $ \fs -> do
            Vk.waitForFences
              (vkDevice ctx)
              (Vector.singleton $ fsFenceInFlight fs)
              True
              maxBound

            (_, imageIndex) <- liftIO $ throwSwapchainOutOfDate $
              Vk.acquireNextImageKHR
                (vkDevice ctx)
                (vkSwapchain $ Resource.get swapchain)
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

            forM_ (framebuffers Vector.! fromIntegral imageIndex)
              $ \(rp, ps, framebuffer) -> do
                let
                  clearValues = Vector.fromList [Vk.Color $ Vk.Float32 (254/255) (243/255) (215/255) 0]
                  renderStartPos = Vk.Offset2D 0 0
                  renderExtent = vkExtent $ Resource.get swapchain
                  renderPassBeginInfo =
                    Vk.RenderPassBeginInfo
                      ()
                      (Resource.get rp)
                      (Resource.get framebuffer)
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

                let pipeline = Vector.head $ Resource.get ps
                Vk.cmdBindPipeline
                  cmdBuffer
                  Vk.PIPELINE_BIND_POINT_GRAPHICS
                  pipeline

                let
                  vertexCount = 3
                  instanceCount = 1
                  firstVertex = 0
                  firstInstance = 0
                Vk.cmdDraw
                  cmdBuffer vertexCount instanceCount firstVertex firstInstance

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
                    (Vector.singleton $ vkSwapchain $ Resource.get swapchain)
                    (Vector.singleton imageIndex)
                    nullPtr
                liftIO $ void
                  $ (throwSwapchainSubOptimal =<<)
                  $ throwSwapchainOutOfDate
                  $ Vk.queuePresentKHR (vkPresentationQueue ctx) presentInfo
          loop
      loop
