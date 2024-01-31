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

      r <- Render.create vkCtx

      pipeline <- Render.mkPipeline r $
        PipelineInfo { pipelineDepthTest = Vk.COMPARE_OP_LESS_OR_EQUAL
                     , pipelineVertexInput = []
                     , pipelineRenderPassLayout = RenderPassLayout
                       { renderPassAttachments = []
                       , renderPassSubpasses = []
                       , renderPassSubpassDependencies = []
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
    -- Renderer.withFrame r $ \frame -> do
    --   Render.submit frame $ Draw { drawShader = shader
    --                              , drawVertexOffset = 0
    --                              , drawInstanceOffset = 0
    --                              , drawInstanceCount = 1
    --                              , drawVertexCount = 3
    --                              }
