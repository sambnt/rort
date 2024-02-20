{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Rort.Examples.Triangle where

import Rort.Window (withWindow, getRequiredExtensions, getWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Rort.Render.Swapchain (vkSwapchain, manageSwapchain)
import Rort.Render.FramesInFlight (withNextFrameInFlight, withFramesInFlight, FrameInFlight (FrameInFlight))
import Rort.Vulkan (withVkShaderModule, withVkCommandBuffers)
import qualified Vulkan.Zero as Vk
import Control.Monad (when)
import Rort.Window.Types (WindowEvent(..))
import Rort.Render (recordFrameData, mkFrameData, finallyPresent, step, withRenderer)
import Rort.Render.Types (Draw(..), SubpassInfo (..), DrawCallPrimitive (..), DrawCall (PrimitiveDraw), RenderPassInfo (RenderPassInfo))
import Data.Acquire (with, allocateAcquire, Acquire)

onceOff :: VkContext -> Acquire RenderPassInfo
onceOff ctx = do
  vertShaderCode <- liftIO $ BS.readFile "data/tri.vert.spv"
  fragShaderCode <- liftIO $ BS.readFile "data/tri.frag.spv"

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
          vertShader
          "main"
          Nothing
      , Vk.PipelineShaderStageCreateInfo
          ()
          Vk.zero
          Vk.SHADER_STAGE_FRAGMENT_BIT
          fragShader
          "main"
          Nothing
      ]

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
  pure renderPassInfo

main :: IO ()
main = do
  let
    width = 800
    height = 600

  withWindow width height "Example: Triangle" $ \win -> do
    windowExts <- getRequiredExtensions win

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

    with (withVkContext cfg win) $ \ctx -> runResourceT $ do
      with (onceOff ctx) $ \renderPassInfo -> do
        let numFramesInFlight = 2

        renderer <-
          withRenderer
            ctx
            numFramesInFlight
            (\swapchain -> mkFrameData ctx swapchain [renderPassInfo])

        let
          loop = do
            step ctx renderer $ \frameData swapchain _descPool cmdPool -> do
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
                Vk.beginCommandBuffer cmdBuffer
                  $ Vk.CommandBufferBeginInfo
                      ()
                      Vk.zero
                      Nothing -- Inheritance info

                recordFrameData cmdBuffer swapchain frameData

                Vk.endCommandBuffer cmdBuffer
                pure cmdBuffer

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
