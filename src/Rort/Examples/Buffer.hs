{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rort.Examples.Buffer where

import Rort.Window (withWindow, getRequiredExtensions, withWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), vkAllocator)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import Rort.Render.Types (SubpassInfo(..), Draw(..), DrawCallIndexed(..), DrawCall (IndexedDraw), RenderPassInfo (..), DrawRenderPass (..), DrawSubpass (DrawSubpass), Attachment (Attachment), AttachmentFormat (SwapchainColorFormat), noUsage, useColorAttachment, Buffer (Buffer))
import Rort.Render (createRenderer, shader, buffer, renderPass, submit, flushBufferAlloc)
import Control.Monad (when)
import Foreign (sizeOf, Word16, castPtr, pokeArray)
import Rort.Window.Types (WindowEvent(..))
import Data.Acquire (allocateAcquire)
import qualified Vulkan.Zero as Vk
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Rort.Allocator as Allocator

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
                             windowExts <> Vector.fromList [ ]
                         , requiredValidationLayers =
                             Vector.fromList [ "VK_LAYER_KHRONOS_validation" ]
                             -- Vector.fromList [ "VK_LAYER_RENDERDOC_Capture" ]
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
        buffer r $ do
          alloc <- Allocator.withBuffer (vkAllocator ctx) Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT vertexBufferSize
          _ <- Allocator.withAllocPtr alloc $ \ptr -> do
            liftIO $ pokeArray (castPtr @() @Float ptr) vertices
          flushBufferAlloc r (vkAllocator ctx) alloc Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT 0 vertexBufferSize
          pure $ Buffer (Allocator.getAllocData alloc) 0 vertexBufferSize
      indexBuffer <-
        buffer r $ do
          alloc <- Allocator.withBuffer (vkAllocator ctx) Vk.BUFFER_USAGE_INDEX_BUFFER_BIT indexBufferSize
          _ <- Allocator.withAllocPtr alloc $ \ptr -> do
            liftIO $ pokeArray (castPtr @() @Word16 ptr) indices
          flushBufferAlloc r (vkAllocator ctx) alloc Vk.BUFFER_USAGE_INDEX_BUFFER_BIT 0 indexBufferSize
          pure $ Buffer (Allocator.getAllocData alloc) 0 indexBufferSize

      let
        subpass0 =
          SubpassInfo { shaderStages     = [vertShader, fragShader]
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
                      , attachmentUsage = noUsage
                                          & useColorAttachment 0
                      , cullMode         = Vk.CULL_MODE_NONE
                      , frontFace        = Vk.FRONT_FACE_CLOCKWISE
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


      let
        renderStep = do
          submit ctx r $ \_ -> do
            let
              draw = Draw
                { drawCall = [ IndexedDraw $ DrawCallIndexed
                               { drawCallIndexedIndexCount = 6
                               , drawCallIndexedInstanceCount = 1
                               , drawCallIndexedFirstIndex = 0
                               , drawCallIndexedVertexOffset = 0
                               , drawCallIndexedFirstInstance = 0
                               }
                             ]
                , drawVertexBuffers = [vertexBuffer]
                , drawIndexBuffers = [(indexBuffer, Vk.INDEX_TYPE_UINT16)]
                , drawDescriptors = []
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
