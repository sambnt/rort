{-# LANGUAGE DataKinds #-}

module Rort.Vulkan where
import qualified Vulkan as Vk
import Control.Monad.Trans.Resource (MonadResource)
import Rort.Util.Resource (Resource)
import qualified Rort.Util.Resource as Resource
import qualified Vulkan.CStruct.Extends as Vk
import Control.Monad (forM_)
import Data.Vector (Vector)
import Data.Word (Word32)
import qualified Vulkan.Zero as Vk

withVkShaderModule
  :: MonadResource m
  => Vk.Device
  -> Vk.ShaderModuleCreateInfo '[]
  -> m (Resource Vk.ShaderModule)
withVkShaderModule device shaderCreateInfo =
  Resource.allocate
    (Vk.createShaderModule device shaderCreateInfo Nothing)
    (\shader -> Vk.destroyShaderModule device shader Nothing)

withVkPipelineLayout
  :: MonadResource m
  => Vk.Device
  -> Vk.PipelineLayoutCreateInfo
  -> m (Resource Vk.PipelineLayout)
withVkPipelineLayout device createInfo =
  Resource.allocate
    (Vk.createPipelineLayout device createInfo Nothing)
    (\pl -> Vk.destroyPipelineLayout device pl Nothing)

withVkRenderPass
  :: MonadResource m
  => Vk.Device
  -> Vk.RenderPassCreateInfo '[]
  -> m (Resource Vk.RenderPass)
withVkRenderPass device createInfo =
  Resource.allocate
    (Vk.createRenderPass device createInfo Nothing)
    (\rp -> Vk.destroyRenderPass device rp Nothing)

withVkGraphicsPipelines
  :: MonadResource m
  => Vk.Device
  -> Vk.PipelineCache
  -> Vector (Vk.GraphicsPipelineCreateInfo '[])
  -> m (Resource (Vector Vk.Pipeline))
withVkGraphicsPipelines device pipelineCache pipelineInfos = do
  Resource.allocate
    (snd <$> Vk.createGraphicsPipelines
       device pipelineCache (Vk.SomeStruct <$> pipelineInfos) Nothing
    )
    (\pipelines ->
       forM_ pipelines $ \pipeline ->
         Vk.destroyPipeline device pipeline Nothing
    )

withVkFramebuffer
  :: MonadResource m
  => Vk.Device
  -> Vk.FramebufferCreateInfo '[]
  -> m (Resource Vk.Framebuffer)
withVkFramebuffer device framebufferInfo = do
  Resource.allocate
    (Vk.createFramebuffer device framebufferInfo Nothing)
    (\fb -> Vk.destroyFramebuffer device fb Nothing)

withVkCommandPool
  :: MonadResource m
  => Vk.Device
  -> Word32
  -> m (Resource Vk.CommandPool)
withVkCommandPool device queueFamilyIx =
  let
    poolInfo =
      Vk.CommandPoolCreateInfo
        Vk.zero
        queueFamilyIx
  in
    Resource.allocate
      (Vk.createCommandPool device poolInfo Nothing)
      (\cmdPool -> Vk.destroyCommandPool device cmdPool Nothing)

withVkCommandBuffers
  :: MonadResource m
  => Vk.Device
  -> Vk.CommandBufferAllocateInfo
  -> m (Resource (Vector Vk.CommandBuffer))
withVkCommandBuffers device allocInfo =
  Resource.allocate
    (Vk.allocateCommandBuffers device allocInfo)
    -- TODO: Is it a good idea to wait for the device to be idle?
    (\cmd ->
      Vk.deviceWaitIdle device
      *> Vk.freeCommandBuffers device (Vk.commandPool allocInfo) cmd
    )
