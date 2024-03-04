{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Rort.Examples.Triangle where

import Rort.Window (withWindow, getRequiredExtensions, withWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Rort.Render.Swapchain (withSwapchain, vkSwapchain, retryOnSwapchainOutOfDate)
import Rort.Render.FramesInFlight (withNextFrameInFlight, withFramesInFlight, FrameInFlight (FrameInFlight))
import Rort.Vulkan (withVkShaderModule, withVkCommandBuffers)
import qualified Vulkan.Zero as Vk
import Control.Monad (when)
import Rort.Window.Types (WindowEvent(..))
import Rort.Render (finallyPresent, createRenderer, shader, renderPassLayout, subpass, submit)
import Rort.Render.Types (Draw(..), SubpassInfo (..), DrawCallPrimitive (..), DrawCall (PrimitiveDraw))
import Data.Acquire (with, Acquire, allocateAcquire)
import qualified Data.ByteString.Lazy as BSL

-- initExample :: VkContext -> Acquire [RenderPassInfo]
-- initExample ctx = do
--   vertShaderCode <- liftIO $ BS.readFile "data/tri.vert.spv"
--   fragShaderCode <- liftIO $ BS.readFile "data/tri.frag.spv"

--   vertShader <-
--     withVkShaderModule (vkDevice ctx)
--       $ Vk.ShaderModuleCreateInfo () Vk.zero vertShaderCode
--   fragShader <-
--     withVkShaderModule (vkDevice ctx)
--       $ Vk.ShaderModuleCreateInfo () Vk.zero fragShaderCode

--   let
--     pipelineShaderStages =
--       [ Vk.PipelineShaderStageCreateInfo
--           ()
--           Vk.zero
--           Vk.SHADER_STAGE_VERTEX_BIT
--           vertShader
--           "main"
--           Nothing
--       , Vk.PipelineShaderStageCreateInfo
--           ()
--           Vk.zero
--           Vk.SHADER_STAGE_FRAGMENT_BIT
--           fragShader
--           "main"
--           Nothing
--       ]

--   let
--     subpassInfo =
--       SubpassInfo { subpassInfoShaderStages = pipelineShaderStages
--                   , subpassInfoDescriptors = []
--                   , subpassInfoVertexBindings = []
--                   , subpassInfoVertexAttributes = []
--                   , subpassInfoDraw = Draw
--                       { drawCall = PrimitiveDraw $ DrawCallPrimitive
--                           { drawCallPrimitiveFirstVertex = 0
--                           , drawCallPrimitiveFirstInstance = 0
--                           , drawCallPrimitiveInstanceCount = 1
--                           , drawCallPrimitiveVertexCount = 3
--                           }
--                       , drawVertexBuffers = []
--                       , drawIndexBuffers = []
--                       }
--                   }
--     renderPassInfo = RenderPassInfo [subpassInfo]

--   pure [renderPassInfo]

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

      (_, ctx) <- allocateAcquire $ withVkContext cfg win

      let numFramesInFlight = 2
      r <- createRenderer ctx numFramesInFlight

      vertShader <-
        shader r Vk.SHADER_STAGE_VERTEX_BIT "main"
          (liftIO $ BSL.readFile "data/tri.vert.spv")
      fragShader <-
        shader r Vk.SHADER_STAGE_FRAGMENT_BIT "main"
          (liftIO $ BSL.readFile "data/tri.frag.spv")

      rpLayout <-
        renderPassLayout r
      subpass0 <-
        subpass r (SubpassInfo { shaderStages     = [vertShader, fragShader]
                               , descriptors      = []
                               , vertexBindings   = []
                               , vertexAttributes = []
                               , layout           = rpLayout
                               , subpassIndex     = 0
                               }
                  )


      let
        renderStep = do
          submit ctx r $ do
            let
              draw = Draw
                { drawCall = PrimitiveDraw $ DrawCallPrimitive
                             { drawCallPrimitiveFirstVertex = 0
                             , drawCallPrimitiveFirstInstance = 0
                             , drawCallPrimitiveInstanceCount = 1
                             , drawCallPrimitiveVertexCount = 3
                             }
                , drawVertexBuffers = []
                , drawIndexBuffers = []
                , drawSubpass = subpass0
                }
            pure [draw]

        loop = do
          renderStep
          shouldContinue <- liftIO $ withWindowEvent win $ \mEv -> do
            case mEv of
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
              Nothing ->
                pure True
          when shouldContinue loop
      loop

      -- let numFramesInFlight = 2
      -- framesInFlight <-
      --   withFramesInFlight (vkDevice ctx) (vkQueueFamilies ctx) numFramesInFlight

      -- framebufferSize <- liftIO $ vkGetFramebufferSize ctx
      -- initialSwapchain <-
      --   allocateAcquire $ withSwapchain ctx framebufferSize Nothing

      -- with (initExample ctx) $ \renderPassInfos -> do
      --   retryOnSwapchainOutOfDate ctx initialSwapchain $ \swapchain -> do
      --     -- BEGIN swapchain-dependent
      --     with (mkFrameData ctx swapchain renderPassInfos) $ \frameDatas -> do
      --       -- rendering a frame
      --       let
      --         loop = do
      --           withNextFrameInFlight (vkDevice ctx) framesInFlight $ \(FrameInFlight fs _descPool cmdPool) -> runResourceT $ do
      --             finallyPresent (vkDevice ctx) (vkGraphicsQueue ctx) (vkPresentationQueue ctx) (vkSwapchain swapchain) fs $ \imageIndex -> do
      --               (_, cmdBuffers) <-
      --                 allocateAcquire $ withVkCommandBuffers
      --                   (vkDevice ctx)
      --                   $ Vk.CommandBufferAllocateInfo
      --                       cmdPool
      --                       -- Primary = can be submitted to queue for execution, can't be
      --                       -- called by other command buffers.
      --                       Vk.COMMAND_BUFFER_LEVEL_PRIMARY
      --                       1 -- count

      --               let cmdBuffer = Vector.head cmdBuffers
      --               Vk.beginCommandBuffer cmdBuffer
      --                 $ Vk.CommandBufferBeginInfo
      --                     ()
      --                     Vk.zero
      --                     Nothing -- Inheritance info

      --               let
      --                 frameData = frameDatas !! fromIntegral imageIndex

      --               recordFrameData cmdBuffer swapchain frameData

      --               Vk.endCommandBuffer cmdBuffer
      --               pure cmdBuffer

      --           shouldContinue <- liftIO $ withWindowEvent win $ \mEv -> do
      --             case mEv of
      --               Just (WindowError err) -> do
      --                 putStrLn $ "Error " <> show err
      --                 closeWindow win
      --                 pure False
      --               Just WindowClose -> do
      --                 putStrLn "Window closing..."
      --                 closeWindow win
      --                 pure False
      --               Just (WindowResize x y) -> do
      --                 putStrLn $ "Window resizing (" <> show x <> ", " <> show y <> ")"
      --                 pure True
      --               Nothing ->
      --                 pure True
      --           when shouldContinue loop
      --       loop
