{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rort.Vulkan where
import qualified Vulkan as Vk
import qualified Vulkan.Core10.MemoryManagement as VkMem
import qualified Vulkan.CStruct.Extends as Vk
import Control.Monad (forM_)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word32)
import qualified Vulkan.Zero as Vk
import Data.Bits ((.&.), shift)
import Control.Monad.IO.Class (liftIO)
import Data.Acquire (Acquire, mkAcquire, with)

withVkShaderModule
  :: Vk.Device
  -> Vk.ShaderModuleCreateInfo '[]
  -> Acquire Vk.ShaderModule
withVkShaderModule device shaderCreateInfo =
  mkAcquire
    (Vk.createShaderModule device shaderCreateInfo Nothing)
    (\shader -> Vk.destroyShaderModule device shader Nothing)

withVkPipelineLayout
  :: Vk.Device
  -> Vk.PipelineLayoutCreateInfo
  -> Acquire Vk.PipelineLayout
withVkPipelineLayout device createInfo =
  mkAcquire
    (Vk.createPipelineLayout device createInfo Nothing)
    (\pl -> Vk.destroyPipelineLayout device pl Nothing)

withVkRenderPass
  :: Vk.Device
  -> Vk.RenderPassCreateInfo '[]
  -> Acquire Vk.RenderPass
withVkRenderPass device createInfo =
  mkAcquire
    (Vk.createRenderPass device createInfo Nothing)
    (\rp -> Vk.destroyRenderPass device rp Nothing)

withVkGraphicsPipelines
  :: Vk.Device
  -> Vk.PipelineCache
  -> Vector (Vk.GraphicsPipelineCreateInfo '[])
  -> Acquire (Vector Vk.Pipeline)
withVkGraphicsPipelines device pipelineCache pipelineInfos = do
  mkAcquire
    (snd <$> Vk.createGraphicsPipelines
       device pipelineCache (Vk.SomeStruct <$> pipelineInfos) Nothing
    )
    (\pipelines ->
       forM_ pipelines $ \pipeline ->
         Vk.destroyPipeline device pipeline Nothing
    )

withVkFramebuffer
  :: Vk.Device
  -> Vk.FramebufferCreateInfo '[]
  -> Acquire Vk.Framebuffer
withVkFramebuffer device framebufferInfo = do
  mkAcquire
    (Vk.createFramebuffer device framebufferInfo Nothing)
    (\fb -> Vk.destroyFramebuffer device fb Nothing)

withVkCommandPool
  :: Vk.Device
  -> Word32
  -> Acquire Vk.CommandPool
withVkCommandPool device queueFamilyIx =
  let
    poolInfo =
      Vk.CommandPoolCreateInfo
        Vk.zero
        queueFamilyIx
  in
    mkAcquire
      (Vk.createCommandPool device poolInfo Nothing)
      (\cmdPool -> Vk.destroyCommandPool device cmdPool Nothing)

withVkCommandBuffers
  :: Vk.Device
  -> Vk.CommandBufferAllocateInfo
  -> Acquire (Vector Vk.CommandBuffer)
withVkCommandBuffers device allocInfo =
  mkAcquire
    (Vk.allocateCommandBuffers device allocInfo)
    -- TODO: Is it a good idea to wait for the device to be idle?
    (\cmd ->
      Vk.deviceWaitIdle device
      *> Vk.freeCommandBuffers device (Vk.commandPool allocInfo) cmd
    )

withVkBuffer
  :: Vk.Device
  -> Vk.BufferCreateInfo '[]
  -> Acquire Vk.Buffer
withVkBuffer device createInfo =
  mkAcquire
    (Vk.createBuffer device createInfo Nothing)
    (\buffer -> Vk.destroyBuffer device buffer Nothing)

withVkBufferMemory
  :: Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.Buffer
  -> Vk.MemoryPropertyFlagBits
  -> Acquire VkMem.DeviceMemory
withVkBufferMemory physDevice device buf memProperties = do
  memRequirements <- liftIO $ Vk.getBufferMemoryRequirements device buf

  typIx <- liftIO $ findMemoryType
    physDevice
    (VkMem.memoryTypeBits memRequirements)
    memProperties

  let
    allocInfo = Vk.MemoryAllocateInfo
      ()
      (VkMem.size memRequirements)
      typIx

  mkAcquire
    (Vk.allocateMemory device allocInfo Nothing)
    (\mem -> Vk.freeMemory device mem Nothing)

withVkDescriptorSetLayout
  :: Vk.Device
  -> Vk.DescriptorSetLayoutCreateInfo '[]
  -> Acquire Vk.DescriptorSetLayout
withVkDescriptorSetLayout device createInfo =
  mkAcquire
    (Vk.createDescriptorSetLayout device createInfo Nothing)
    (\dsl -> Vk.destroyDescriptorSetLayout device dsl Nothing)

withVkDescriptorPool
  :: Vk.Device
  -> Vk.DescriptorPoolCreateInfo '[]
  -> Acquire Vk.DescriptorPool
withVkDescriptorPool device createInfo =
  mkAcquire
    (Vk.createDescriptorPool device createInfo Nothing)
    (\pool -> Vk.destroyDescriptorPool device pool Nothing)

withVkDescriptorSet
  :: Vk.Device
  -> Vk.DescriptorSetAllocateInfo '[]
  -> Acquire [Vk.DescriptorSet]
withVkDescriptorSet device allocInfo =
  Vector.toList <$> mkAcquire
    (Vk.allocateDescriptorSets device allocInfo)
    -- TODO: Not recommended to free descriptor sets, just free pool
    (Vk.freeDescriptorSets device (Vk.descriptorPool allocInfo))

withFence
  :: Vk.Device
  -> Vk.FenceCreateInfo '[]
  -> Acquire Vk.Fence
withFence logicalDevice fenceInfo =
  mkAcquire
    (Vk.createFence logicalDevice fenceInfo Nothing)
    (\fence -> Vk.destroyFence logicalDevice fence Nothing)

withSemaphore
  :: Vk.Device
  -> Vk.SemaphoreCreateInfo '[]
  -> Acquire Vk.Semaphore
withSemaphore logicalDevice semaphoreInfo =
  mkAcquire
    (Vk.createSemaphore logicalDevice semaphoreInfo Nothing)
    (\sem -> Vk.destroySemaphore logicalDevice sem Nothing)

findMemoryType
  :: Vk.PhysicalDevice
  -> Word32
  -> Vk.MemoryPropertyFlagBits
  -> IO Word32
findMemoryType physDevice typeFilter properties = do
  memProperties <- Vk.getPhysicalDeviceMemoryProperties physDevice

  let
    typs = flip foldMap (zip [0..] $ Vector.toList $ Vk.memoryTypes memProperties) $ \(ix :: Word32, typ) -> do
      let suitableMemType = typeFilter .&. (1 `shift` fromIntegral ix) /= 0
      if suitableMemType && Vk.propertyFlags typ == properties
      then [ix]
      else mempty

  case typs of
    [] -> error $ "Unable to find suitable memory type for buffer: (" <> show typeFilter <> ", " <> show (Vector.length $ Vk.memoryTypes memProperties) <> ")"
    (t:_ts) -> pure t

copyBuffer
  :: Vk.Device
  -- ^ Logical device
  -> Vk.CommandPool
  -- ^ Command buffer pool
  -> Vk.Queue
  -- ^ Queue to submit transfer operation on
  -> Vk.DeviceSize
  -- ^ Size of data to copy from src buffer
  -> Vk.Buffer
  -- ^ Src buffer
  -> Vk.Buffer
  -- ^ Dst buffer
  -> IO ()
copyBuffer device cmdPool que size bufferSrc bufferDst = do
  let
    allocInfo =
      Vk.CommandBufferAllocateInfo
        cmdPool
        Vk.COMMAND_BUFFER_LEVEL_PRIMARY -- level
        1 -- count

  with (withVkCommandBuffers device allocInfo) $ \commandBuffers -> do
    let
      commandBuffer = Vector.head commandBuffers
      beginInfo = Vk.CommandBufferBeginInfo
        ()
        Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
        Nothing -- inheritance info

    liftIO $ Vk.beginCommandBuffer commandBuffer beginInfo

    let
      copyRegion =
        Vk.BufferCopy
          0 -- src offset
          0 -- dst offset
          size -- size

    liftIO $
      Vk.cmdCopyBuffer
        commandBuffer
        bufferSrc
        bufferDst
        (Vector.singleton copyRegion)

    liftIO $ Vk.endCommandBuffer commandBuffer
    let
      submitInfo = Vk.SubmitInfo
        ()
        mempty -- wait semaphores
        mempty -- wait dst stage mask
        (Vector.singleton (Vk.commandBufferHandle commandBuffer)) -- commandBuffers
        mempty -- signal semaphores

    liftIO $ Vk.queueSubmit que (Vector.singleton $ Vk.SomeStruct submitInfo) Vk.NULL_HANDLE
    liftIO $ Vk.queueWaitIdle que
