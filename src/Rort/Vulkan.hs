{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rort.Vulkan where
import qualified Vulkan as Vk
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Vulkan.Core10.MemoryManagement as VkMem
import Rort.Util.Resource (Resource)
import qualified Rort.Util.Resource as Resource
import qualified Vulkan.CStruct.Extends as Vk
import Control.Monad (forM_)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word32)
import qualified Vulkan.Zero as Vk
import Data.Bits ((.&.), shift)
import Control.Monad.IO.Class (liftIO)
import Data.Acquire (Acquire, mkAcquire)

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

withVkBuffer
  :: MonadResource m
  => Vk.Device
  -> Vk.BufferCreateInfo '[]
  -> m (Resource Vk.Buffer)
withVkBuffer device createInfo =
  Resource.allocate
    (Vk.createBuffer device createInfo Nothing)
    (\buffer -> Vk.destroyBuffer device buffer Nothing)

withVkBufferMemory
  :: MonadResource m
  => Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.Buffer
  -> Vk.MemoryPropertyFlagBits
  -> m (Resource VkMem.DeviceMemory)
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

  Resource.allocate
    (Vk.allocateMemory device allocInfo Nothing)
    (\mem -> Vk.freeMemory device mem Nothing)

withVkDescriptorSetLayout
  :: MonadResource m
  => Vk.Device
  -> Vk.DescriptorSetLayoutCreateInfo '[]
  -> m (Resource Vk.DescriptorSetLayout)
withVkDescriptorSetLayout device createInfo =
  Resource.allocate
    (Vk.createDescriptorSetLayout device createInfo Nothing)
    (\dsl -> Vk.destroyDescriptorSetLayout device dsl Nothing)

withVkDescriptorPool
  :: MonadResource m
  => Vk.Device
  -> Vk.DescriptorPoolCreateInfo '[]
  -> m (Resource Vk.DescriptorPool)
withVkDescriptorPool device createInfo =
  Resource.allocate
    (Vk.createDescriptorPool device createInfo Nothing)
    (\pool -> Vk.destroyDescriptorPool device pool Nothing)

withVkDescriptorSet
  :: MonadResource m
  => Vk.Device
  -> Vk.DescriptorSetAllocateInfo '[]
  -> m (Resource [Vk.DescriptorSet])
withVkDescriptorSet device allocInfo = fmap Vector.toList
  <$> Resource.allocate
        (Vk.allocateDescriptorSets device allocInfo)
        -- TODO: Not recommended to free descriptor sets, just free pool
        (Vk.freeDescriptorSets device (Vk.descriptorPool allocInfo))

withFence
  :: MonadResource m
  => Vk.Device
  -> Vk.FenceCreateInfo '[]
  -> m (Resource Vk.Fence)
withFence logicalDevice fenceInfo =
    Resource.allocate
      (Vk.createFence logicalDevice fenceInfo Nothing)
      (\fence -> Vk.destroyFence logicalDevice fence Nothing)

withSemaphore
  :: MonadResource m
  => Vk.Device
  -> Vk.SemaphoreCreateInfo '[]
  -> m (Resource Vk.Semaphore)
withSemaphore logicalDevice semaphoreInfo =
    Resource.allocate
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
copyBuffer device cmdPool que size bufferSrc bufferDst = runResourceT $ do
  let
    allocInfo =
      Vk.CommandBufferAllocateInfo
        cmdPool
        Vk.COMMAND_BUFFER_LEVEL_PRIMARY -- level
        1 -- count

  commandBuffers <- withVkCommandBuffers device allocInfo
  let
    commandBuffer = Vector.head $ Resource.get commandBuffers
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
