{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Rort.Examples.Triangle where

import Rort.Window (withWindow, getRequiredExtensions, getWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..), graphicsQueueFamilies)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Rort.Render.Swapchain (withSwapchain, vkSwapchain, throwSwapchainOutOfDate, throwSwapchainSubOptimal, retryOnSwapchainOutOfDate)
import Rort.Render.FramesInFlight (withNextFrameInFlight, withFramesInFlight, fsSemaphoreRenderFinished, fsSemaphoreImageAvailable, fsFenceInFlight, FrameInFlight (FrameInFlight))
import Rort.Vulkan (withVkShaderModule, withVkCommandBuffers, withVkCommandPool)
import qualified Vulkan.Zero as Vk
import qualified Rort.Util.Resource as Resource
import qualified Vulkan.CStruct.Extends as Vk
import Control.Monad (when)
import Data.Functor (void)
import Foreign (nullPtr)
import Rort.Window.Types (WindowEvent(..))
import Rort.Render (recordFrameData, mkFrameData)
import Rort.Render.Types (Draw(..), SubpassInfo (..), DrawCallPrimitive (..), DrawCall (PrimitiveDraw), RenderPassInfo (RenderPassInfo))

main :: IO ()
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
      framesInFlight <-
        withFramesInFlight (vkDevice ctx) numFramesInFlight

      framebufferSize <- liftIO $ vkGetFramebufferSize ctx
      initialSwapchain <-
        withSwapchain ctx framebufferSize Nothing

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

      cmdPool <-
        withVkCommandPool
          (vkDevice ctx)
          (NE.head . graphicsQueueFamilies $ vkQueueFamilies ctx)

      retryOnSwapchainOutOfDate ctx initialSwapchain $ \swapchain -> do
        -- BEGIN swapchain-dependent
        let
          subpassInfo =
            SubpassInfo { subpassInfoShaderStages = pipelineShaderStages
                        , subpassInfoDescriptors = []
                        , subpassInfoVertexBindings = []
                        , subpassInfoVertexAttributes = []
                        , subpassInfoDraw = Draw
                            { drawCall = PrimitiveDraw $ DrawCallPrimitive
                                { drawCallPrimitiveFirstVertex = 0
                                , drawCallPrimitiveFirstInstance = 0
                                , drawCallPrimitiveInstanceCount = 1
                                , drawCallPrimitiveVertexCount = 3
                                }
                            , drawVertexBuffers = []
                            , drawIndexBuffers = []
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
          loop = do
            withNextFrameInFlight (vkDevice ctx) framesInFlight $ \(FrameInFlight fs _descPool) -> runResourceT $ do
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

              recordFrameData cmdBuffer swapchain frameData

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
