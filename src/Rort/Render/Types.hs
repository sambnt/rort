{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
  = Draw { drawCall          :: DrawCall
         , drawVertexBuffers :: [BufferRef]
         , drawIndexBuffers  :: [(BufferRef, Vk.IndexType)]
         , drawSubpass       :: Handle Subpass
         }

data RenderPassLayoutInfo = RenderPassLayoutInfo

data RenderPassLayout
  = RenderPassLayout { renderPass :: Vk.RenderPass
                     , framebuffers :: [Vk.Framebuffer]
                     }

data SubpassInfo
  = SubpassInfo { shaderStages     :: [ Handle Shader ]
                , descriptors      :: [Vk.DescriptorSetLayout]
                , vertexBindings   :: [Vk.VertexInputBindingDescription]
                , vertexAttributes :: [Vk.VertexInputAttributeDescription]
                , layout           :: Handle RenderPassLayout
                , subpassIndex     :: Word32
                }

-- output
data Subpass
  = Subpass { subpassPipeline       :: Vk.Pipeline
            , subpassPipelineLayout :: Vk.PipelineLayout
            }

data ShaderInfo
  = ShaderInfo { shaderStage :: Vk.ShaderStageFlagBits
               , entryFn     :: BS.ByteString
               , dat         :: Acquire BSL.ByteString
               }

data Shader
  = Shader { pipelineShaderStage :: Vk.PipelineShaderStageCreateInfo '[]
           }

data BufferInfo
  = BufferInfo { usage :: Vk.BufferUsageFlagBits
               , dat   :: Acquire (Word64, BSL.ByteString)
               }

data Buffer
  = Buffer { buffer :: Vk.Buffer
           , offset :: Word64
           }

data Handle a where
  ShaderHandle :: Deferred ShaderInfo Shader -> Handle Shader
  BufferHandle :: Deferred BufferInfo Buffer -> Handle Buffer
  RenderPassLayoutHandle
    :: Deferred RenderPassLayoutInfo (ReleaseKey, RenderPassLayout)
    -> Handle RenderPassLayout
  SubpassHandle
    :: Deferred SubpassInfo (ReleaseKey, Subpass) -> Handle Subpass

unsafeGetHandle :: MonadIO m => Handle a -> m a
unsafeGetHandle (ShaderHandle h)           = unsafeGet h
unsafeGetHandle (BufferHandle h)           = unsafeGet h
unsafeGetHandle (RenderPassLayoutHandle h) = snd <$> unsafeGet h
unsafeGetHandle (SubpassHandle h)          = snd <$> unsafeGet h
