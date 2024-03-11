{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

module Rort.Render.Types where

import Data.Word (Word32, Word64)
import Data.Int (Int32)
import qualified Vulkan as Vk
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Acquire (Acquire)
import Rort.Util.Defer (Deferred, unsafeGet)
import Control.Monad.Trans.Resource (ReleaseKey)
import Control.Monad.IO.Class (MonadIO)
import Foreign (Storable)
import qualified Data.Vector as Vector
import qualified Vulkan.Zero as Vk
import qualified Vulkan.Core10.Pass as VkPass
import Rort.Render.Swapchain (Swapchain, vkSurfaceFormat, vkDepthFormat)
import qualified Vulkan.Extensions.VK_KHR_surface as VkFormat

data DrawCallIndexed
  = DrawCallIndexed { drawCallIndexedIndexCount    :: Word32
                    , drawCallIndexedInstanceCount :: Word32
                    , drawCallIndexedFirstIndex    :: Word32
                    , drawCallIndexedVertexOffset  :: Int32
                    , drawCallIndexedFirstInstance :: Word32
                    }

data DrawCallPrimitive
  = DrawCallPrimitive { drawCallPrimitiveFirstVertex :: Word32
                      , drawCallPrimitiveFirstInstance :: Word32
                      , drawCallPrimitiveInstanceCount :: Word32
                      , drawCallPrimitiveVertexCount :: Word32
                      }

data DrawCall = IndexedDraw DrawCallIndexed
              | PrimitiveDraw DrawCallPrimitive

data Draw
  = Draw { drawCall           :: [DrawCall]
         , drawVertexBuffers  :: [Handle Buffer]
         , drawIndexBuffers   :: [(Handle Buffer, Vk.IndexType)]
         , drawDescriptors    :: [[DrawDescriptor]]
         }

data DrawRenderPass = DrawRenderPass { drawRenderPass  :: Handle RenderPass
                                     , drawClearValues :: [ Vk.ClearValue ]
                                     , drawSubpasses   :: [ DrawSubpass ]
                                     }

data DrawSubpass = DrawSubpass { subpassDraws :: [ Draw ] }

data DrawDescriptor = DescriptorUniform Buffer
                    | DescriptorTexture (Handle Texture)

data AttachmentFormat = SwapchainColorFormat
                      | SwapchainDepthFormat
                      | VkFormat Vk.Format

-- TODO: data Attachment = RenderAttachment
                      -- | DepthAttachment
                      -- | Attachment ... usage

data Attachment = Attachment { format        :: AttachmentFormat
                             , loadOp        :: Vk.AttachmentLoadOp
                             , storeOp       :: Vk.AttachmentStoreOp
                             , initialLayout :: Vk.ImageLayout
                             , finalLayout   :: Vk.ImageLayout
                             }

toAttachmentDescription :: Swapchain -> Attachment -> Vk.AttachmentDescription
toAttachmentDescription sc attach =
  Vk.AttachmentDescription
    Vk.zero
    (case attach.format of
       SwapchainColorFormat ->
         VkFormat.format $ vkSurfaceFormat sc
       SwapchainDepthFormat ->
         vkDepthFormat sc
       VkFormat fmt ->
         fmt
    )
    Vk.SAMPLE_COUNT_1_BIT
    attach.loadOp
    attach.storeOp
    Vk.ATTACHMENT_LOAD_OP_DONT_CARE                  -- Stencil load op
    Vk.ATTACHMENT_STORE_OP_DONT_CARE                 -- Stencil store op
    attach.initialLayout
    attach.finalLayout

data RenderPassInfo
  = RenderPassInfo { attachments         :: [Attachment]
                   , subpasses           :: [SubpassInfo]
                   , subpassDependencies :: [Vk.SubpassDependency]
                   }

data RenderPass
  = RenderPass { renderPass :: Vk.RenderPass
               , framebuffers :: [Vk.Framebuffer]
               , subpasses :: [Subpass]
               }

noUsage :: Vk.SubpassDescription
noUsage =
  Vk.SubpassDescription
    Vk.zero
    Vk.PIPELINE_BIND_POINT_GRAPHICS
    Vector.empty
    Vector.empty
    Vector.empty
    Nothing
    Vector.empty

useColorAttachment
  :: Word32 -> Vk.SubpassDescription -> Vk.SubpassDescription
useColorAttachment ix s =
  s { VkPass.colorAttachments =
        VkPass.colorAttachments s
        <> Vector.fromList [ Vk.AttachmentReference
                               ix
                               Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                           ]
    }

useDepthAttachment
  :: Word32 -> Vk.SubpassDescription -> Vk.SubpassDescription
useDepthAttachment ix s =
  s { VkPass.depthStencilAttachment =
        Just ( Vk.AttachmentReference
                 ix
                 Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
             )
    }

data SubpassInfo
  = SubpassInfo { shaderStages     :: [Handle Shader]
                , descriptors      :: [[Vk.DescriptorSetLayoutBinding]]
                , vertexBindings   :: [Vk.VertexInputBindingDescription]
                , vertexAttributes :: [Vk.VertexInputAttributeDescription]
                , attachmentUsage  :: Vk.SubpassDescription
                , cullMode         :: Vk.CullModeFlags
                , frontFace        :: Vk.FrontFace
                }

-- output
data Subpass
  = Subpass { subpassPipeline       :: Vk.Pipeline
            , subpassPipelineLayout :: Vk.PipelineLayout
            , subpassSetLayouts     :: [Vk.DescriptorSetLayout]
            }

data ShaderInfo
  = ShaderInfo { shaderStage :: Vk.ShaderStageFlagBits
               , entryFn     :: BS.ByteString
               , dat         :: Acquire BSL.ByteString
               }

data Shader
  = Shader { pipelineShaderStage :: Vk.PipelineShaderStageCreateInfo '[]
           }

data BufferInfo a
  = BufferInfo { usage :: Vk.BufferUsageFlagBits
               , dat   :: Acquire (Word64, [a])
               }

data Buffer
  = Buffer { buffer :: Vk.Buffer
           , offset :: Word64
           , sz     :: Word64
           }

data TextureInfo a
  = TextureInfo { format :: Vk.Format
                , width  :: Word32
                , height :: Word32
                , size   :: Word64
                , dat    :: [a]
                }

data Texture = Texture { img :: Vk.Image
                       , imgView :: Vk.ImageView
                       , sampler :: Vk.Sampler
                       }

data Handle a where
  ShaderHandle :: Deferred ShaderInfo Shader -> Handle Shader
  BufferHandle
    :: forall x. Storable x
    => Deferred (BufferInfo x) Buffer -> Handle Buffer
  RenderPassHandle
    :: Deferred RenderPassInfo (ReleaseKey, RenderPass)
    -> Handle RenderPass
  -- SubpassHandle
  --   :: Deferred SubpassInfo (ReleaseKey, Subpass) -> Handle Subpass
  TextureHandle
    :: forall x. Storable x
    => Deferred (Acquire (TextureInfo x)) Texture
    -> Handle Texture

unsafeGetHandle :: MonadIO m => Handle a -> m a
unsafeGetHandle (ShaderHandle h)           = unsafeGet h
unsafeGetHandle (BufferHandle h)           = unsafeGet h
unsafeGetHandle (RenderPassHandle h) = snd <$> unsafeGet h
-- unsafeGetHandle (SubpassHandle h)          = snd <$> unsafeGet h
unsafeGetHandle (TextureHandle h)          = unsafeGet h
