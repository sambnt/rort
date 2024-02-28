{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Rort.Render.Types where

import Data.Word (Word32, Word64)
import Data.Int (Int32)
import qualified Vulkan as Vk
import Rort.Util.Defer (Deferred)
import qualified Data.ByteString.Lazy as BSL
import Data.Acquire (Acquire)
import qualified Data.ByteString as BS
import Control.Monad.Trans.Resource (ReleaseKey)

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

-- data Draw
--   = Draw { drawCall :: DrawCall
--          , drawVertexBuffers :: [BufferRef]
--          , drawIndexBuffers  :: [(BufferRef, Vk.IndexType)]
--          }

data SubpassInfo
  = SubpassInfo { subpassInfoShaderStages     :: [Handle Shader]
                , subpassInfoDescriptors      :: [Vk.DescriptorSetLayout]
                , subpassInfoVertexBindings   :: [Vk.VertexInputBindingDescription]
                , subpassInfoVertexAttributes :: [Vk.VertexInputAttributeDescription]
                , subpassInfoRenderPassLayout :: Handle RenderPass
                , subpassInfoIx               :: Word32
                -- , subpassInfoDraw             :: Draw
                }

data RenderPassInfo = RenderPassInfo

-- output
data Subpass
  = Subpass { subpassPipeline :: Vk.Pipeline
            , subpassPipelineLayout :: Vk.PipelineLayout
            -- , subpassDraw     :: Draw
            }

data RenderPass
  = RenderPass { renderPass :: Vk.RenderPass
               -- , renderPassSubpasses :: [Subpass]
               , framebuffers :: [Vk.Framebuffer]
               }

data BufferInfo
  = BufferInfo { usage :: Vk.BufferUsageFlagBits
               , dat :: Acquire (Word64, BSL.ByteString)
               }

data Buffer
  = Buffer { buffer :: Vk.Buffer
           , offset :: Word64
           }

data ShaderInfo
  = ShaderInfo { shaderStage :: Vk.ShaderStageFlagBits
               , entryFn     :: BS.ByteString
               , dat         :: Acquire BSL.ByteString
               }

data Shader
  = Shader { pipelineShaderStage :: Vk.PipelineShaderStageCreateInfo '[]
           }


data Draw = Draw { drawSubpass :: Handle Subpass
                 , drawVertexBuffers :: [ Handle Buffer ]
                 , drawIndexBuffer :: Maybe (Handle Buffer)
                 , drawCall :: DrawCall
                 }


data Handle a where
  ShaderHandle     :: Deferred ShaderInfo Shader                       -> Handle Shader
  BufferHandle     :: Deferred BufferInfo Buffer                       -> Handle Buffer
  SubpassHandle    :: Deferred SubpassInfo (ReleaseKey, Subpass)       -> Handle Subpass
  RenderPassHandle :: Deferred RenderPassInfo (ReleaseKey, RenderPass) -> Handle RenderPass
