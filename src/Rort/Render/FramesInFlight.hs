module Rort.Render.FramesInFlight where

import Control.Concurrent.STM (TQueue, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import qualified Vulkan as Vk
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadMask, bracket, finally)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Word (Word32)
import Control.Monad (replicateM_)
import qualified Vulkan.Zero as Vk
import Rort.Vulkan (withFence, withSemaphore, withVkDescriptorPool, withVkCommandPool)
import Rort.Util.Resource (Resource)
import qualified Rort.Util.Resource as Resource
import qualified Data.Vector as Vector
import Rort.Vulkan.Context (QueueFamilies, graphicsQueueFamilies)
import qualified Data.List.NonEmpty as NE

data FrameSync
  = FrameSync { fsFenceInFlight           :: Vk.Fence
              , fsSemaphoreImageAvailable :: Vk.Semaphore
              , fsSemaphoreRenderFinished :: Vk.Semaphore
              }

data FrameInFlight = FrameInFlight { fifFrameSynce     :: FrameSync
                                   , fifDescriptorPool :: Vk.DescriptorPool
                                   , fifCommandPool    :: Vk.CommandPool
                                   }

data FramesInFlight
  = FramesInFlight { framesInFlightFrameSync :: TQueue FrameInFlight
                   }

type NumFramesInFlight = Word32

withNextFrameInFlight
  :: ( MonadMask m
     , MonadIO m
     )
  => Vk.Device
  -> FramesInFlight
  -> (FrameInFlight -> m r)
  -> m r
withNextFrameInFlight device s f =
  bracket (liftIO . STM.atomically . STM.readTQueue $ framesInFlightFrameSync s)
          (liftIO . STM.atomically . STM.writeTQueue (framesInFlightFrameSync s))
          (\fif ->
             f fif
               `finally`
                 Vk.resetDescriptorPool device (fifDescriptorPool fif) Vk.zero
               `finally`
                 Vk.resetCommandPool device (fifCommandPool fif) Vk.zero
          )

withFramesInFlight
  :: MonadResource m
  => Vk.Device
  -> QueueFamilies
  -> NumFramesInFlight
  -> m FramesInFlight
withFramesInFlight device queueFamilies numFramesInFlight = do
  framesQue <- liftIO newTQueueIO
  replicateM_ (fromIntegral numFramesInFlight) $ do
    fs <- Resource.get <$> withFrameSync device
    -- TODO: We just hard-code some large sizes for the pool. In the future we
    -- might analyse the render graph to allocate the right number of descriptor
    -- sets, OR we could use a free list of descriptor pools.
    let
      maxSets = 1024
      maxUniformBuffers = 8192
      maxImageSamplers = 8192
    descPool <-
      fmap Resource.get $ withVkDescriptorPool
        device
        $ Vk.DescriptorPoolCreateInfo
            ()
            Vk.zero
            maxSets -- max sets we can allocate from this pool
            ( Vector.fromList
                [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER maxUniformBuffers
                , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER maxImageSamplers
                ]
            )
    -- Allocate our command pool for this frame
    cmdPool <-
      Resource.get <$> withVkCommandPool
        device
        (NE.head . graphicsQueueFamilies $ queueFamilies)
    liftIO
      $ STM.atomically
      $ STM.writeTQueue framesQue (FrameInFlight fs descPool cmdPool)

  pure $ FramesInFlight framesQue

withFrameSync
  :: MonadResource m
  => Vk.Device
  -> m (Resource FrameSync)
withFrameSync logicalDevice = do
  let
    -- Create the fence pre-signalled so the first time we wait on the fence in
    -- drawing, we don't wait forever.
    fenceInfo = Vk.FenceCreateInfo () Vk.FENCE_CREATE_SIGNALED_BIT
    semaphoreInfo = Vk.SemaphoreCreateInfo () Vk.zero
  inFlightFence <- withFence logicalDevice fenceInfo
  renderFinishedSem <- withSemaphore logicalDevice semaphoreInfo
  imageAvailableSem <- withSemaphore logicalDevice semaphoreInfo
  pure $ FrameSync
    <$> inFlightFence
    <*> imageAvailableSem
    <*> renderFinishedSem
