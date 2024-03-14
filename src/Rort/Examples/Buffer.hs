{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rort.Examples.Buffer where

import Rort.Window (withWindow, getRequiredExtensions, withWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import Rort.Render.Types (SubpassInfo(..), Draw(..), DrawCallIndexed(..), DrawCall (IndexedDraw))
import Rort.Render (createRenderer, shader, buffer, renderPassLayout, subpass, submit)
import Control.Monad (when)
import Foreign (sizeOf, Word16)
import Rort.Window.Types (WindowEvent(..))
import Data.Acquire (allocateAcquire)
import qualified Data.ByteString.Lazy as BSL

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
                             Vector.fromList [ "VK_LAYER_KHRONOS_validation"
                                             , "VK_LAYER_RENDERDOC_Capture"
                                             ]
                         , applicationInfo =
                             Vk.ApplicationInfo
                               (Just "Example: Buffer")  -- application name
                               (Vk.MAKE_API_VERSION 1 0 0) -- application version
                               (Just "No engine")          -- engine name
                               (Vk.MAKE_API_VERSION 1 0 0) -- engine version
                               (Vk.MAKE_API_VERSION 1 3 0) -- Vulkan API version (patch version ignored)
                         }

      (_, ctx) <- allocateAcquire $ withVkContext cfg win

      let numFramesInFlight = 2
      r <- createRenderer ctx numFramesInFlight

      vertShader <-
        shader r Vk.SHADER_STAGE_VERTEX_BIT "main"
          (liftIO $ BSL.readFile "data/vertexBuffers.vert.spv")
      fragShader <-
        shader r Vk.SHADER_STAGE_FRAGMENT_BIT "main"
          (liftIO $ BSL.readFile "data/tri.frag.spv")

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

      vertexBuffer <-
        buffer r Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
          $ pure (vertexBufferSize, vertices)
      indexBuffer <-
        buffer r Vk.BUFFER_USAGE_INDEX_BUFFER_BIT
          $ pure (indexBufferSize, indices)

      rpLayout <-
        renderPassLayout r
      subpass0 <-
        subpass r (SubpassInfo { shaderStages     = [vertShader, fragShader]
                               , descriptors      = []
                               , vertexBindings   =
                                   [ Vk.VertexInputBindingDescription
                                     0 -- first vertex buffer bound
                                     (fromIntegral $ sizeOf (undefined :: Float) * 5)
                                     Vk.VERTEX_INPUT_RATE_VERTEX
                                   ]
                               , vertexAttributes =
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
                               , layout           = rpLayout
                               , subpassIndex     = 0
                               }
                  )


      let
        renderStep = do
          submit ctx r $ \_swapchain -> do
            let
              draw = Draw
                { drawCall = IndexedDraw $ DrawCallIndexed
                    { drawCallIndexedIndexCount = 6
                    , drawCallIndexedInstanceCount = 1
                    , drawCallIndexedFirstIndex = 0
                    , drawCallIndexedVertexOffset = 0
                    , drawCallIndexedFirstInstance = 0
                    }
                , drawVertexBuffers = [vertexBuffer]
                , drawIndexBuffers = [(indexBuffer, Vk.INDEX_TYPE_UINT16)]
                , drawSubpass = subpass0
                , drawUniformBuffers = []
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
