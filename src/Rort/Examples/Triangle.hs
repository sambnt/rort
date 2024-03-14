{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Rort.Examples.Triangle where

import Rort.Window (withWindow, getRequiredExtensions, withWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Rort.Window.Types (WindowEvent(..))
import Rort.Render (createRenderer, subpass, shader, renderPassLayout, submit)
import Rort.Render.Types (Draw(..), SubpassInfo (..), DrawCallPrimitive (..), DrawCall (PrimitiveDraw))
import Data.Acquire (allocateAcquire)
import qualified Data.ByteString.Lazy as BSL

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
                             Vector.fromList [ "VK_LAYER_KHRONOS_validation"
                                             , "VK_LAYER_RENDERDOC_Capture"
                                             ]
                         , applicationInfo =
                             Vk.ApplicationInfo
                               (Just "Example: Triangle")  -- application name
                               (Vk.MAKE_API_VERSION 1 0 0) -- application version
                               (Just "No engine")          -- engine name
                               (Vk.MAKE_API_VERSION 1 0 0) -- engine version
                               -- TODO: Don't let user specify this - we know
                               -- what features we're using, the user doesn't.
                               (Vk.MAKE_API_VERSION 1 3 0) -- Vulkan API version (patch version ignored)
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
          submit ctx r $ \_swapchain -> do
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
                , drawDescriptors = []
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
