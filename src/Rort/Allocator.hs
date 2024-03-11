{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

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
  -> m Vma.Allocator
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
  -> Acquire Vma.Allocator
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

type AllocationInfo a = (a, Vma.Allocation, Vma.AllocationInfo)

data Allocation a
  = DeviceAllocation (AllocationInfo a)
  | StagingAllocation (AllocationInfo Vk.Buffer) (AllocationInfo a)

withBuffer
  :: Allocator
  -> Vk.BufferUsageFlagBits
  -> Word64
  -> Acquire (Allocation Vk.Buffer)
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
        (usage .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT)
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

    staging <- mkAcquire
      (Vma.createBuffer allocator stagingBufferCreateInfo stagingAllocCreateInfo)
      (\(b, bufAlloc, _) -> Vma.destroyBuffer allocator b bufAlloc)

    pure $ StagingAllocation staging (buf, alloc, allocInfo)

withAllocPtr :: Allocation a -> (Ptr () -> r) -> r
withAllocPtr (DeviceAllocation (_, _alloc, allocInfo)) f =
  f allocInfo.mappedData
withAllocPtr (StagingAllocation (_, _alloc, allocInfo) _) f =
  f allocInfo.mappedData

-- TODO: Finish this
flush
  :: MonadIO m
  => Vma.Allocator
  -> Allocation a
  -> ("offset" Vk.::: Vk.DeviceSize)
  -> Vk.DeviceSize -> m ()
flush allocator (DeviceAllocation (_buf, alloc, _allocInfo)) =
  Vma.flushAllocation allocator alloc
flush allocator (StagingAllocation (_buf, alloc, _allocInfo) _) =
  Vma.flushAllocation allocator alloc

getAllocData :: Allocation a -> a
getAllocData (DeviceAllocation (buf, _alloc, _allocInfo)) = buf
getAllocData (StagingAllocation _ (buf, _, _)) = buf

-- TODO: Finish this
getAllocOffset :: Allocation a -> Word64
getAllocOffset _ = 0

requiresBufferCopy :: Allocation a -> Maybe (Vk.Buffer, a)
requiresBufferCopy (DeviceAllocation _) =
  Nothing
requiresBufferCopy (StagingAllocation (staging, _, _) (device, _, _)) =
  Just (staging, device)

withImage
  :: Allocator
  -> Vk.ImageCreateInfo '[]
  -> Word64
  -> Acquire (Allocation Vk.Image)
withImage allocator createInfo imgSize = do
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

  (img, alloc, allocInfo) <- mkAcquire
    (Vma.createImage allocator createInfo allocCreateInfo)
    (\(img, alloc, _allocInfo) -> Vma.destroyImage allocator img alloc)

  memPropFlags
     <- liftIO $ Vma.getAllocationMemoryProperties allocator alloc

  if memPropFlags .&. Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
      == Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
  then
    pure $ DeviceAllocation (img, alloc, allocInfo)
  else do
    let
      stagingBufferCreateInfo =
          Vk.BufferCreateInfo
            ()
            Vk.zero
            imgSize
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

    staging <- mkAcquire
      (Vma.createBuffer allocator stagingBufferCreateInfo stagingAllocCreateInfo)
      (\(b, bufAlloc, _) -> Vma.destroyBuffer allocator b bufAlloc)

    pure $ StagingAllocation staging (img, alloc, allocInfo)

withImageAttachment
  :: Allocator
  -> Vk.ImageCreateInfo '[]
  -> Acquire (Allocation Vk.Image)
withImageAttachment allocator createInfo = do
  let
    allocCreateInfo =
      Vma.AllocationCreateInfo
        -- Flags
        Vma.ALLOCATION_CREATE_DEDICATED_MEMORY_BIT
        Vma.MEMORY_USAGE_AUTO
        Vk.zero -- required flags
        Vk.zero -- preferred flags
        Vk.zero -- Accept any memory types that meet the requirements
        Vk.NULL_HANDLE -- pool to create allocation in
        nullPtr -- userdata
        1.0 -- priority

  (img, alloc, allocInfo) <- mkAcquire
    (Vma.createImage allocator createInfo allocCreateInfo)
    (\(img, alloc, _allocInfo) -> Vma.destroyImage allocator img alloc)

  pure $ DeviceAllocation (img, alloc, allocInfo)
