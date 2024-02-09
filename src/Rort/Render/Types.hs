{-# LANGUAGE DataKinds #-}
module Rort.Render.Types where

import Data.Word (Word32, Word64)
import Data.Int (Int32)
import qualified Vulkan as Vk

data BufferRef = BufferRef { bufRefBuffer :: Vk.Buffer
                           , bufRefOffset :: Word64
                           }

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
  = Draw { drawCall :: DrawCall
         , drawVertexBuffers :: [BufferRef]
         , drawIndexBuffers  :: [(BufferRef, Vk.IndexType)]
         -- TODO: Multiple descriptors
         , drawDescriptor :: Maybe Vk.Buffer
         }

data SubpassInfo
  = SubpassInfo { subpassInfoShaderStages     :: [Vk.PipelineShaderStageCreateInfo '[]]
                , subpassInfoDescriptors      :: [Vk.DescriptorSetLayoutBinding]
                , subpassInfoVertexBindings   :: [Vk.VertexInputBindingDescription]
                , subpassInfoVertexAttributes :: [Vk.VertexInputAttributeDescription]
                , subpassInfoDraw             :: Draw
                }

data RenderPassInfo
  = RenderPassInfo { renderPassInfoSubpasses :: [SubpassInfo]
                   }

-- output
data Subpass
  = Subpass { subpassPipeline            :: Vk.Pipeline
            , subpassPipelineLayout      :: Vk.PipelineLayout
            , subpassDescriptorSetLayout :: Maybe Vk.DescriptorSetLayout
            , subpassDraw                :: Draw
            }

data RenderPass
  = RenderPass { renderPass :: Vk.RenderPass
               , renderPassSubpasses :: [Subpass]
               }

data FrameData
  = FrameData { frameRenderPasses :: [(Vk.Framebuffer, RenderPass)]
              , frameDescriptorPool :: Vk.DescriptorPool
              }
