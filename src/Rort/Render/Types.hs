module Rort.Render.Types where

import qualified Vulkan as Vk
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)
import qualified Vulkan.Zero as Vk

data Handle a = Handle Int
  deriving (Eq, Ord)

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

toVkAttachmentDescription
  :: Vk.Format
  -> Vk.Format
  -> AttachmentInfo
  -> Vk.AttachmentDescription
toVkAttachmentDescription swapchainColorFormat swapchainDepthFormat (SwapchainAttachment desc) =
  Vk.AttachmentDescription
    Vk.zero
    (case attachmentFormat desc of
       SwapchainColorFormat -> swapchainColorFormat
       SwapchainDepthFormat -> swapchainDepthFormat
       VulkanFormat fmt     -> fmt
    )
    (attachmentSamples desc)
    (attachmentLoadOp desc)
    (attachmentStoreOp desc)
    (attachmentStencilLoadOp desc)
    (attachmentStencilStoreOp desc)
    (attachmentInitialLayout desc)
    (attachmentFinalLayout desc)

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
