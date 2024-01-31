module Rort.Render.Types where

import qualified Vulkan as Vk
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)

data Handle a = Handle Int

data AttachmentFormat = VulkanFormat Vk.Format
                      | SwapchainColorFormat
                      | SwapchainDepthFormat

data AttachmentDescription
  = AttachmentDescription { attachmentFormat         :: AttachmentFormat
                          , attachmentSamples        :: Vk.SampleCountFlagBits
                          , attachmentLoadOp         :: Vk.AttachmentLoadOp
                          , attachmentStoreOp        :: Vk.AttachmentStoreOp
                          , attachmentStencilLoadOp  :: Vk.AttachmentLoadOp
                          , attachmentStencilStoreOp :: Vk.AttachmentStoreOp
                          , attachmentInitialLayout  :: Vk.ImageLayout
                          , attachmentFinalLayout    :: Vk.ImageLayout
                          }

data AttachmentInfo
  = SwapchainAttachment AttachmentDescription
  -- | ImageAttachment { attachmentDescription :: AttachmentDescription
  --                   , attachmentUsage       :: Vk.ImageUsageFlagBits
  --                   , attachmentAspect      :: Vk.ImageAspectFlagBits
  --                   }

data RenderPassLayout =
  RenderPassLayout { renderPassAttachments :: [AttachmentInfo]
                   , renderPassSubpasses :: [Vk.SubpassDescription]
                   , renderPassSubpassDependencies :: [Vk.SubpassDependency]
                   }

data PipelineInfo =
  PipelineInfo { pipelineDepthTest :: Vk.CompareOp
               , pipelineVertexInput :: [()]
               , pipelineRenderPassLayout :: RenderPassLayout
               }

data Pipeline

data ShaderStageDesc
  = ShaderStageDesc { shaderStageCode      :: ByteString
                    , shaderStageEntryFunc :: Text
                    }

data ShaderInfo
  = ShaderInfo { shaderVertex     :: ShaderStageDesc
               , shaderFragment   :: ShaderStageDesc
               , shaderPipeline   :: Handle Pipeline
               }

data Shader

data Draw
  = Draw { drawShader         :: Shader
         , drawVertexOffset   :: Word32
         , drawInstanceOffset :: Word32
         , drawInstanceCount  :: Word32
         , drawVertexCount    :: Word32
         }
