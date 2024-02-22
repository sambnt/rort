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
