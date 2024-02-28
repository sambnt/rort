module Rort.Examples.Render where
import Rort.Vulkan.Context (VkSettings(..), withVkContext)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Data.Acquire (allocateAcquire)
import qualified Rort.Render as Renderer
import Rort.Window (withWindow, getRequiredExtensions, withWindowEvent, closeWindow)
import Control.Monad.Trans.Resource (runResourceT)
import Rort.Render (shader, vertexBuffer, indexBuffer, subpass, renderPass)
import qualified Data.ByteString.Lazy as BSL
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word16)
import Foreign (sizeOf)
import qualified Data.Binary as Binary
import Rort.Render.Types (SubpassInfo(..), RenderPassInfo (RenderPassInfo))
import Rort.Window.Types (WindowEvent(..))
import Control.Monad (when)
import UnliftIO (race_)

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

      (_, ctx) <- allocateAcquire $ withVkContext cfg win

      let numFramesInFlight = 2
      r <- Renderer.create ctx numFramesInFlight

      vertShader <-
        shader r Vk.SHADER_STAGE_VERTEX_BIT "main" $
          liftIO (BSL.readFile "data/vertexBuffers.vert.spv")
      fragShader <-
        shader r Vk.SHADER_STAGE_FRAGMENT_BIT "main" $
          liftIO (BSL.readFile "data/tri.frag.spv")

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

      v <- vertexBuffer r (pure (vertexBufferSize, Binary.encode vertices))
      i <- indexBuffer r (pure (indexBufferSize, Binary.encode indices))

      renderPass1 <- renderPass r RenderPassInfo
      subpass1 <-
        subpass r $ SubpassInfo { subpassInfoShaderStages = [ vertShader, fragShader ]
                                , subpassInfoDescriptors = []
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
                                , subpassInfoRenderPassLayout = renderPass1
                                , subpassInfoIx = 0
                                }

      let
        renderLoop = do
          Renderer.step ctx r $ do
            undefined

          renderLoop

        eventLoop = do
          shouldContinue <- liftIO $ withWindowEvent win $ \mEv -> do
            case mEv of
              Just (WindowError err) -> do
                putStrLn $ "Error " <> show err
                closeWindow win
                pure False
              Just WindowClose -> do
                -- putStrLn "Window closing..."
                closeWindow win
                pure False
              Just (WindowResize _x _y) -> do
                -- putStrLn $ "Window resizing (" <> show x <> ", " <> show y <> ")"
                pure True
              Nothing ->
                pure True
          when shouldContinue eventLoop

      race_ eventLoop renderLoop
