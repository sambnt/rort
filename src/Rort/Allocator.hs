{-# LANGUAGE OverloadedRecordDot #-}

module Rort.Allocator where

import qualified VulkanMemoryAllocator as Vma
import Foreign (nullPtr, castFunPtr, (.|.))
import qualified Vulkan.Zero as Vk
import qualified Vulkan.Core10.DeviceInitialization as Vk
import Data.Word (Word32, Word64)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Vulkan as Vk
import qualified Vulkan.Dynamic as VkDynamic
import qualified Data.Vector as Vector
import Foreign.Ptr (Ptr)
import Data.Acquire (Acquire, mkAcquire)
import Data.Bits ((.&.))

type Allocator = Vma.Allocator

create
  :: MonadIO m
  => Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.Instance
  -> Word32
  -- ^ Vulkan API Version
  -> m Allocator
create physicalDevice device inst vulkanApiVersion =
  let
    vulkanFuncs :: Vma.VulkanFunctions
    vulkanFuncs =
      Vk.zero
        { Vma.vkGetInstanceProcAddr =
            castFunPtr
            $ VkDynamic.pVkGetInstanceProcAddr inst.instanceCmds
        , Vma.vkGetDeviceProcAddr =
            castFunPtr
            $ VkDynamic.pVkGetDeviceProcAddr device.deviceCmds
        }

    createInfo = Vma.AllocatorCreateInfo
      Vk.zero                                  -- flags
      (Vk.physicalDeviceHandle physicalDevice) -- physicalDevice
      (Vk.deviceHandle device)                 -- device
      0                                        -- preferredLargeHeapBlockSize, default = 256MiB
      Nothing                                  -- allocationCallbacks
      Nothing                                  -- deviceMemoryCallbacks
      nullPtr                                  -- heapSizeLimit
      (Just vulkanFuncs)                       -- vulkanFunctions
      (Vk.instanceHandle inst)                 -- instance
      vulkanApiVersion                         -- vulkanApiVersion
      nullPtr                                  -- typeExternalMemoryHandleTypes
  in
    Vma.createAllocator createInfo

destroy :: MonadIO m => Allocator -> m ()
destroy = Vma.destroyAllocator

withAllocator
  :: Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.Instance
  -> Word32
  -> Acquire Allocator
withAllocator physicalDevice device inst vulkanApiVersion =
  mkAcquire
    (create physicalDevice device inst vulkanApiVersion)
    destroy

withUniformBuffer
  :: Allocator
  -> Word64
  -> Acquire (Vk.Buffer, Ptr ())
withUniformBuffer allocator size = do
  -- See
  -- https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/usage_patterns.html#usage_patterns_advanced_data_uploadi
  let
    bufferCreateInfo =
      Vk.BufferCreateInfo
        ()
        Vk.zero
        size
        (Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT)
        Vk.SHARING_MODE_EXCLUSIVE
        Vector.empty -- queue family indices, ignored.

    allocCreateInfo =
      Vma.AllocationCreateInfo
        -- Flags
        ( Vma.ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
        .|. Vma.ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT
        .|. Vma.ALLOCATION_CREATE_MAPPED_BIT
        )
        Vma.MEMORY_USAGE_AUTO
        Vk.zero -- required flags
        Vk.zero -- preferred flags
        Vk.zero -- Accept any memory types that meet the requirements
        Vk.NULL_HANDLE -- pool to create allocation in
        nullPtr -- userdata
        0 -- priority

  (buf, alloc, allocInfo) <- mkAcquire
    (Vma.createBuffer allocator bufferCreateInfo allocCreateInfo)
    (\(buffer, alloc, _allocInfo) -> Vma.destroyBuffer allocator buffer alloc)

  _memPropFlags
     <- liftIO $ Vma.getAllocationMemoryProperties allocator alloc

  -- TODO: Handle non-host-visible memory
  pure (buf, allocInfo.mappedData)

type AllocationInfo = (Vk.Buffer, Vma.Allocation, Vma.AllocationInfo)

data Allocation = DeviceAllocation AllocationInfo
                | StagingAllocation AllocationInfo AllocationInfo

flush _ = pure ()

withAllocPtr a (DeviceAllocation (_buf, _alloc, allocInfo)) f =
  f allocInfo.mappedData
withAllocPtr a (StagingAllocation (_buf, _alloc, allocInfo) _) f =
  f allocInfo.mappedData

getAllocBuffer (DeviceAllocation (buf, _alloc, _allocInfo)) = buf
getAllocBuffer (StagingAllocation _ (buf, _, _)) = buf

getAllocOffset _ = 0

-- flush
--   :: Allocator
--   -> Allocation
--   -> IO ()
-- flush a (DeviceAllocation _) = pure ()
-- flush a (StagingAllocation (stagingBuf,stagingAlloc,_) (deviceBuf, _, _)) = do
--   Vma.flushAllocation a stagingAlloc 0 Vk.WHOLE_SIZE
--   _ cmdCopyBuffer
--   -- wait idle
--   pure ()

-- flushCmd ctx a (DeviceAllocation _ ) _ = pure ()
-- flushCmd ctx a (StagingAllocation (stagingBuf, stagingAlloc, stagingAllocInfo) (deviceBuf, deviceAlloc, deviceAllocInfo)) cmdBuffer = do
--   Vma.flushAllocation a stagingAlloc 0 Vk.WHOLE_SIZE

--   Vk.beginCommandBuffer cmdBuffer

--   let
--     copyRegion = Vk.BufferCopy
--       stagingAllocInfo.offset -- src offset
--       deviceAllocInfo.offset  -- dst offset
--       stagingAllocInfo.size   -- size

--   Vk.cmdCopyBuffer cmdBuffer stagingBuf deviceBuf (Vector.singleton copyRegion)

--   if vkGraphicsQueue ctx == vkTransferQueue ctx
--   then do
--     let
--       memoryBarrier = Vk.MemoryBarrier2
--         Vk.PIPELINE_STAGE_2_TRANSFER_BIT_KHR -- src stage mask
--         Vk.ACCESS_2_MEMORY_WRITE_BIT_KHR -- src access mask
--         (Vk.PIPELINE_STAGE_2_VERTEX_INPUT_BIT .|. PIPELINE_STAGE_2_INDEX_INPUT_BIT) -- dst stage mask, TODO: Finer grained barrier based on type of allocation
--         Vk.ACCESS_2_MEMORY_READ_BIT_KHR -- dst access mask

--       dependencyInfo = Vk.DependencyInfo
--         Vk.zero -- dependency flags
--         (Vector.singleton memoryBarrier) -- memory barriers
--         Vector.empty -- buffer memory barriers
--         Vector.empty -- image memory barriers

--     Vk.cmdPipelineBarrier2KHR cmdBuffer dependencyInfo

--     Vk.endCommandBuffer cmdBuffer

--     let
--       submitInfo = Vk.SubmitInfo
--         () -- next
--         Vector.empty -- wait semaphores
--         Vector.empty -- wait dst stage mask
--         (Vector.singleton $ Vk.commandBufferHandle cmdBuffer)
--         Vector.empty -- signal semaphores

--     Vk.queueSubmit (vkGraphicsQueue ctx) () Vk.NULL_HANDLE
--   else do
--     let
--       bufferMemoryBarrier = Vk.BufferMemoryBarrier2
--         Vk.PIPELINE_STAGE_2_TRANSFER_BIT_KHR -- src stage mask
--         Vk.ACCESS_2_MEMORY_WRITE_BIT_KHR -- src access mask
--         Vk.zero -- dst stage mask
--         Vk.zero -- dst access mask
--         (vkTransferQueueIx ctx) -- src queue family ix
--         (vkGraphicsQueueIx ctx) -- dst queue family ix
--         deviceBuf -- buffer
--         deviceAllocInfo.offset -- offset
--         deviceAllocInfo.size -- size

--       dependencyInfo = Vk.DependencyInfo
--         Vk.zero -- dependency flags
--         Vector.empty -- memory barriers
--         (Vector.singleton bufferMemoryBarrier) -- buffer memory barriers
--         Vector.empty -- image memory barriers

--     Vk.cmdPipelineBarrier2KHR cmdBuffer dependencyInfo

--     Vk.endCommandBuffer cmdBuffer

--     let
--       submitInfo = Vk.SubmitInfo
--         () -- next
--         Vector.empty -- wait semaphores
--         Vector.empty -- wait dst stage mask
--         (Vector.singleton $ Vk.commandBufferHandle cmdBuffer)
--         Vector.empty -- signal semaphores

--     undefined

--   -- cmd copy buffer
--   -- pipeline barrier
--   -- vkCmdPipelineBarrier: VK_ACCESS_HOST_WRITE_BIT --> VK_ACCESS_TRANSFER_READ_BIT

-- -- Use this for efficiency, as we can just use one pipeline barrier for all
-- -- allocations.
-- flushManyCmd = do
--   undefined



withBuffer
  :: Allocator
  -> Vk.BufferUsageFlagBits
  -> Word64
  -> Acquire Allocation
withBuffer allocator usage sz = do
  let
    allocCreateInfo =
      Vma.AllocationCreateInfo
        -- Flags
        ( Vma.ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
        .|. Vma.ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT
        .|. Vma.ALLOCATION_CREATE_MAPPED_BIT
        )
        Vma.MEMORY_USAGE_AUTO
        Vk.zero -- required flags
        Vk.zero -- preferred flags
        Vk.zero -- Accept any memory types that meet the requirements
        Vk.NULL_HANDLE -- pool to create allocation in
        nullPtr -- userdata
        0 -- priority

    bufferCreateInfo =
      Vk.BufferCreateInfo
        ()
        Vk.zero
        sz
        usage
        Vk.SHARING_MODE_EXCLUSIVE -- TODO: gfx/transfer queue
        Vector.empty -- queue family indices, ignored.

  (buf, alloc, allocInfo) <- mkAcquire
    (Vma.createBuffer allocator bufferCreateInfo allocCreateInfo)
    (\(buf, alloc, _allocInfo) -> Vma.destroyBuffer allocator buf alloc)

  memPropFlags
     <- liftIO $ Vma.getAllocationMemoryProperties allocator alloc

  if memPropFlags .&. Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
      == Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
  then
    pure $ DeviceAllocation (buf, alloc, allocInfo)
  else do
    let
      stagingBufferCreateInfo =
        Vk.BufferCreateInfo
          ()
          Vk.zero
          sz
          Vk.BUFFER_USAGE_TRANSFER_SRC_BIT
          Vk.SHARING_MODE_EXCLUSIVE
          Vector.empty
      stagingAllocCreateInfo =
        Vma.AllocationCreateInfo
          -- Flags
          ( Vma.ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
          .|. Vma.ALLOCATION_CREATE_MAPPED_BIT
          )
          Vma.MEMORY_USAGE_AUTO
          Vk.zero -- required flags
          Vk.zero -- preferred flags
          Vk.zero -- Accept any memory types that meet the requirements
          Vk.NULL_HANDLE -- pool to create allocation in
          nullPtr -- userdata
          0 -- priority

    (stagingBuf, stagingAlloc, stagingAllocInfo) <- mkAcquire
      (Vma.createBuffer allocator stagingBufferCreateInfo stagingAllocCreateInfo)
      (\(b, bufAlloc, _) -> Vma.destroyBuffer allocator b bufAlloc)

    pure $ StagingAllocation (stagingBuf, stagingAlloc, stagingAllocInfo)
                             (buf, alloc, allocInfo)

  undefined
