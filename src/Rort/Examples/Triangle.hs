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
import Rort.Render (createRenderer, shader, renderPass, submit)
import Rort.Render.Types (Draw(..), SubpassInfo (..), DrawCallPrimitive (..), DrawCall (PrimitiveDraw), RenderPassInfo (..), DrawRenderPass (..), DrawSubpass (DrawSubpass), Attachment (Attachment), AttachmentFormat (SwapchainColorFormat), noUsage, useColorAttachment)
import Data.Acquire (allocateAcquire)
import qualified Data.ByteString.Lazy as BSL
import qualified Vulkan.Zero as Vk
import Data.Function ((&))

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
                             -- Vector.fromList [ "VK_LAYER_KHRONOS_validation" ]
                             Vector.empty
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

      let
        subpass0 =
          SubpassInfo { shaderStages     = [vertShader, fragShader]
                      , descriptors      = []
                      , vertexBindings   = []
                      , vertexAttributes = []
                      , attachmentUsage  = noUsage
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
                { drawCall = PrimitiveDraw $ DrawCallPrimitive
                             { drawCallPrimitiveFirstVertex = 0
                             , drawCallPrimitiveFirstInstance = 0
                             , drawCallPrimitiveInstanceCount = 1
                             , drawCallPrimitiveVertexCount = 3
                             }
                , drawVertexBuffers = []
                , drawIndexBuffers = []
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
