module Rort.Examples.Triangle where

import Rort.Window (withWindow, getRequiredExtensions)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..))
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Rort.Render.Renderer as Render
import Rort.Render.Types (PipelineInfo(..), RenderPassLayout (..), ShaderInfo (..), ShaderStageDesc (..), Draw (..))

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

      vkCtx <- withVkContext cfg win

      vertShaderCode <- liftIO $ BS.readFile "data/tri.vert.spv"
      fragShaderCode <- liftIO $ BS.readFile "data/tri.frag.spv"

      let numFramesInFlight = 2
      r <- Render.create vkCtx numFramesInFlight

      pipeline <- Render.mkPipeline r $
        PipelineInfo { pipelineDepthTest = Vk.COMPARE_OP_LESS_OR_EQUAL
                     , pipelineVertexInput = []
                     , pipelineRenderPassLayout = RenderPassLayout
                       { renderPassAttachments = [
                           SwapchainAttachment
                             $ AttachmentDescription
                                 SwapchainColorFormat
                                 Vk.SAMPLE_COUNT_1_BIT -- samples
                                 Vk.ATTACHMENT_LOAD_OP_CLEAR -- load op
                                 Vk.ATTACHMENT_STORE_OP_STORE -- store op
                                 Vk.ATTACHMENT_LOAD_OP_DONT_CARE -- stencil load op
                                 Vk.ATTACHMENT_STORE_OP_DONT_CARE -- stencil store op
                                 Vk.IMAGE_LAYOUT_UNDEFINED -- initial image layout
                                 Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
                           ]
                       , renderPassSubpasses = [
                           Vk.SubpassDescription
                             Vk.zero
                             Vk.PIPELINE_BIND_POINT_GRAPHICS
                             Vector.empty -- input attachments
                             -- color attachments
                             ( Vector.singleton $ Vk.AttachmentReference
                                 0 -- attachment ix
                                 Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                             )
                             Vector.empty -- resolve attachments
                             Nothing -- depth stencil attachments
                             Vector.empty -- preserve attachments
                           ]
                       , renderPassSubpassDependencies = [
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
                     }

      shader <- Render.mkShader r $
        ShaderInfo { shaderVertex = ShaderStageDesc { shaderStageCode = vertShaderCode
                                                    , shaderStageEntryFunc = "main"
                                                    }
                   , shaderFragment = ShaderStageDesc { shaderStageCode = fragShaderCode
                                                      , shaderStageEntryFunc = "main"
                                                      }
                   , shaderPipeline = pipeline
                   }

      liftIO $ print "Hello world!"
    Renderer.withFrame r $ do
       [ Draw { drawShader = shader
              , drawVertexOffset = 0
              , drawInstanceOffset = 0
              , drawInstanceCount = 1
              , drawVertexCount = 3
              }
       ]
