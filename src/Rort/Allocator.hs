{-# LANGUAGE OverloadedRecordDot #-}

module Rort.Allocator where

import Foreign (nullPtr, castFunPtr)
import qualified Vulkan.Zero as Vk
import qualified VulkanMemoryAllocator as Vma
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Vulkan as Vk
import qualified Control.Monad.Trans.Resource as ResourceT
import Control.Monad.Trans.Resource (MonadResource)
import Rort.Util.Resource (Resource)
import qualified Rort.Util.Resource as Resource
import qualified Data.Vector as Vector
import qualified Vulkan.Dynamic as VkDynamic
import qualified Vulkan.Core10.Device as VkDevice
import qualified Vulkan.Core10.DeviceInitialization as VkInst
import Data.Bits ((.|.), (.&.))
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr)

data Allocator = Allocator { vmaAllocator :: Vma.Allocator
                           }

create
  :: MonadIO f
  => Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.Instance
  -> Word32
  -- ^ Vulkan API Version
  -> f Allocator
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

    createInfo =
      Vma.AllocatorCreateInfo
        Vk.zero                                  -- flags
        (Vk.physicalDeviceHandle physicalDevice) -- physicalDevice
        (Vk.deviceHandle device)                 -- device
        0                                        -- preferredLargeHeapBlockSize, default = 256MiB
        Nothing                                  -- allocationCallbacks
        Nothing                                  -- deviceMemoryCallbacks
        nullPtr                                  -- heapSizeLimit
        (Just vulkanFuncs)                       -- vulkanFunction
        (Vk.instanceHandle inst)                 -- instance
        vulkanApiVersion                         -- vulkanApiVersion
        nullPtr                                  -- typeExternalMemoryHandleTypes
  in
    Allocator <$> Vma.createAllocator createInfo

withUniformBuffer
  :: MonadResource m
  => Allocator
  -> Word64
  -> m (Resource (Vk.Buffer, Ptr ()))
withUniformBuffer allocator size = do
  -- See
  -- https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/usage_patterns.html#usage_patterns_advanced_data_uploading
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
        ( Vma.ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
          .|. Vma.ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT
          .|. Vma.ALLOCATION_CREATE_MAPPED_BIT
        )
        Vma.MEMORY_USAGE_AUTO
        Vk.zero -- required flags
        Vk.zero -- preferred flags
        Vk.zero -- Accept any memory type that meets the requirements
        Vk.NULL_HANDLE -- pool to create allocation in
        nullPtr -- userdata
        0 -- priority

  resource <- Resource.allocate
    (Vma.createBuffer (vmaAllocator allocator) bufferCreateInfo allocCreateInfo)
    (\(buffer, alloc, _allocInfo) ->
       Vma.destroyBuffer (vmaAllocator allocator) buffer alloc
    )

  let (_buf, alloc, allocInfo) = Resource.get resource

  memPropFlags
    <- liftIO $ Vma.getAllocationMemoryProperties (vmaAllocator allocator) alloc

  liftIO $ do
    print allocInfo
    print memPropFlags
    print $
      (memPropFlags .&. Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT)
      == Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT

  pure $ do
    (buffer, _alloc, info) <- resource
    pure (buffer, info.mappedData)


destroy :: MonadIO m => Allocator -> m ()
destroy = Vma.destroyAllocator . vmaAllocator

withAllocator
  :: MonadResource m
  => Vk.PhysicalDevice
  -> Vk.Device
  -> Vk.Instance
  -> Word32
  -- ^ Vulkan API Version
  -> m (Resource Allocator)
withAllocator physicalDevice logicalDevice inst vulkanApiVersion =
  Resource.allocate
    (create physicalDevice logicalDevice inst vulkanApiVersion)
    destroy
