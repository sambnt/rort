{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rort.Examples.Texture where

import Rort.Window (withWindow, getRequiredExtensions, withWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import Rort.Render.Swapchain (Swapchain (vkExtent))
import Linear (M44, Quaternion)
import qualified Linear
import qualified Torsor
import Data.Function ((&))
import qualified Rort.Allocator as Allocator
import qualified Chronos
import Control.Lens ((%~))
import Data.Acquire (allocateAcquire)
import Rort.Render.Types (SubpassInfo(..), Draw(..), DrawCallIndexed(..), DrawCall (IndexedDraw), Buffer (Buffer), TextureInfo (TextureInfo), DrawDescriptor (..))
import Rort.Render (createRenderer, shader, buffer, renderPassLayout, subpass, submit, texture, evalTexture)
import Control.Monad (when)
import Foreign (sizeOf, Word8, Word16, pokeArray, castPtr, peekArray)
import Rort.Window.Types (WindowEvent(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Image.STB as STB
import Data.Bitmap (withBitmap)

main :: IO ()
main = do
  let
    width = 800
    height = 600

  withWindow width height "Example: Texture" $ \win -> do
    windowExts <- getRequiredExtensions win

    runResourceT $ do
      let
        cfg = VkSettings { requiredExtensions =
                             windowExts <> Vector.fromList [ ]
                         , requiredValidationLayers =
                             Vector.fromList [ "VK_LAYER_KHRONOS_validation"
                                             , "VK_LAYER_RENDERDOC_Capture"
                                             ]
                         , applicationInfo =
                             Vk.ApplicationInfo
                               (Just "Example: Texture")   -- application name
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
          (liftIO $ BSL.readFile "data/texture.vert.spv")
      fragShader <-
        shader r Vk.SHADER_STAGE_FRAGMENT_BIT "main"
          (liftIO $ BSL.readFile "data/texture.frag.spv")

      tex <-
        texture r $ do
          t <- liftIO $ loadTexture "data/texture.jpg"
          (imgData :: [Word8], (w, h)) <-
            liftIO $ withBitmap t $ \(w,h) _chan _padd ptr -> do
              dat <- peekArray (w * h * 4) ptr
              pure (dat, (w, h))
          let imgDataSize = fromIntegral $ w * h * 4
          pure $
            TextureInfo
              Vk.FORMAT_R8G8B8A8_SRGB
              (fromIntegral w)
              (fromIntegral h)
              imgDataSize
              imgData

      _ <- evalTexture ctx r tex

      let
        vertices :: [Float]
        vertices = [ -0.5, -0.5, 1, 0, 0, 1, 0
                   ,  0.5, -0.5, 0, 1, 0, 0, 0
                   ,  0.5,  0.5, 0, 0, 1, 0, 1
                   , -0.5,  0.5, 1, 1, 1, 1, 1
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
                               , descriptors      = [
                                   [ Vk.DescriptorSetLayoutBinding
                                       0 -- binding in shader
                                       Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                       1 -- descriptor count
                                       Vk.SHADER_STAGE_VERTEX_BIT
                                       Vector.empty -- immutable samplers
                                   , Vk.DescriptorSetLayoutBinding
                                       1 -- binding in shader
                                       Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                                       1 -- descriptor count
                                       Vk.SHADER_STAGE_FRAGMENT_BIT
                                       Vector.empty -- immutable samplers
                                   ]
                                 ]
                               , vertexBindings   =
                                   [ Vk.VertexInputBindingDescription
                                     0 -- first vertex buffer bound
                                     (fromIntegral $ sizeOf (undefined :: Float) * 7)
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
                                   , Vk.VertexInputAttributeDescription
                                       2 -- location (color)
                                       0 -- binding
                                       Vk.FORMAT_R32G32_SFLOAT
                                       (fromIntegral $ sizeOf (undefined :: Float) * 5)
                                   ]
                               , layout           = rpLayout
                               , subpassIndex     = 0
                               }
                  )

      startTime <- liftIO Chronos.now


      let
        renderStep = do
          currentTime <- liftIO Chronos.now
          submit ctx r $ \swapchain -> do
            let
              uniformBufferSize =
                fromIntegral $ 3 * sizeOf(undefined :: M44 Float)
            (_, (uniformBuffer, uniformBufferPtr)) <-
              allocateAcquire $ Allocator.withUniformBuffer
                    (vkAllocator ctx)
                    uniformBufferSize
            let
              newUniformBufferData =
                getUniformBufferData
                  startTime
                  currentTime
                  ( fromIntegral . Extent2D.width $ vkExtent swapchain
                  , fromIntegral . Extent2D.height $ vkExtent swapchain
                  )
            liftIO $ pokeArray (castPtr @() @(M44 Float) $ uniformBufferPtr) newUniformBufferData

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
                , drawDescriptors = [
                     [ DescriptorUniform $ Buffer uniformBuffer 0 uniformBufferSize
                     , DescriptorTexture tex
                     ]
                  ]
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

getUniformBufferData
  :: Chronos.Time -> Chronos.Time -> (Int, Int) -> [M44 Float]
getUniformBufferData startTime currentTime (w, h) = do
  -- This is an abstract time unit, not seconds, not nanoseconds, it's abstract.
  let
    timePassed = currentTime `Torsor.difference` startTime

    asFloat = fromIntegral . Chronos.getTimespan

    rotQ :: Linear.Quaternion Float
    rotQ = Linear.axisAngle (Linear.V3 0 0 1) (asFloat timePassed * ((pi / 2) / asFloat Chronos.second))

    model :: M44 Float
    model = Linear.transpose $ Linear.mkTransformation rotQ (Linear.V3 0 0 0)

    view :: M44 Float
    view = Linear.transpose $ Linear.lookAt
      (Linear.V3 2 2 2) -- Eye position
      (Linear.V3 0 0 0) -- Looking at
      (Linear.V3 0 0 1) -- Up direction

    proj :: M44 Float
    proj = Linear.perspective
      (45.0 * pi / 180.0) -- FOV y in radians
      (fromIntegral w / fromIntegral h)
      0.1 -- Near plane
      10  -- Far plane
      & Linear.transpose
      & Linear._y . Linear._y %~ (* (-1))

    in
      [model, view, proj]

loadTexture :: FilePath -> IO STB.Image
loadTexture path = do
  let numComponents = 4 -- Load with four components (alpha channel)
  eImg <- STB.loadImage' path numComponents
  case eImg of
    Left err -> error $ "Failed to load image, error was: '" <> show err <> "'"
    Right img -> pure img
