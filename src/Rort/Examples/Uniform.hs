{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Rort.Examples.Uniform where

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
import Rort.Render.Types (SubpassInfo(..), Draw(..), DrawCallIndexed(..), DrawCall (IndexedDraw), Buffer (Buffer), DrawDescriptor (DescriptorUniform), DrawRenderPass (..), DrawSubpass (DrawSubpass), RenderPassInfo (..), Attachment (..), AttachmentFormat (SwapchainColorFormat), noUsage, useColorAttachment)
import Rort.Render (createRenderer, shader, buffer, renderPass, submit)
import Control.Monad (when)
import Foreign (sizeOf, Word16, pokeArray, castPtr)
import Rort.Window.Types (WindowEvent(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Vulkan.Zero as Vk

main :: IO ()
main = do
  let
    width = 800
    height = 600

  withWindow width height "Example: Uniform" $ \win -> do
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
                               (Just "Example: Uniform")  -- application name
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
          (liftIO $ BSL.readFile "data/uniformBuffer.vert.spv")
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

      let
        subpass0 =
          SubpassInfo { shaderStages     = [vertShader, fragShader]
                      , descriptors      = [
                          [ Vk.DescriptorSetLayoutBinding
                              0 -- binding in shader
                              Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                              1 -- descriptor count
                              Vk.SHADER_STAGE_VERTEX_BIT
                              Vector.empty -- immutable samplers
                          ]
                        ]
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
                      , attachmentUsage = noUsage
                                          & useColorAttachment 0
                      }

      rp <-
        renderPass r $
          RenderPassInfo { attachments =
                             [ Attachment
                                 SwapchainColorFormat
                                 Vk.ATTACHMENT_LOAD_OP_CLEAR
                                 Vk.ATTACHMENT_STORE_OP_STORE
                                 Vk.IMAGE_LAYOUT_UNDEFINED
                                 Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
                             ]
                         , subpasses = [ subpass0 ]
                         , subpassDependencies = [
                             Vk.SubpassDependency
                               Vk.SUBPASS_EXTERNAL -- src subpass
                               0 -- dst subpass
                               Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT -- src stage mask
                               Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT -- dst stage mask
                               Vk.zero -- src access mask
                               Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT -- dst access mask
                               Vk.zero -- dependency flags
                           ]
                         }

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
                    [ DescriptorUniform $ Buffer uniformBuffer 0 uniformBufferSize ]
                  ]
                }
            pure [ DrawRenderPass
                     { drawRenderPass = rp
                     , drawClearValues =
                         [ Vk.Color $ Vk.Float32 0 0 0 0
                         ]
                     , drawSubpasses = [ DrawSubpass [ draw ] ]
                     }
                 ]

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
