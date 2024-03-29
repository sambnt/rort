module Rort.Render.FramesInFlight where

import Control.Concurrent.STM (TQueue, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import qualified Vulkan as Vk
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Safe (MonadMask, bracket, finally, mask, onException, uninterruptibleMask_)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, release)
import Data.Word (Word32)
import Control.Monad (replicateM_)
import qualified Vulkan.Zero as Vk
import Rort.Vulkan (withFence, withSemaphore, withVkDescriptorPool, withVkCommandPool)
import qualified Data.Vector as Vector
import Rort.Vulkan.Context (QueueFamilies, graphicsQueueFamilies)
import qualified Data.List.NonEmpty as NE
import Data.Acquire (Acquire, allocateAcquire)

data FrameSync
  = FrameSync { fsFenceInFlight           :: Vk.Fence
              , fsSemaphoreImageAvailable :: Vk.Semaphore
              , fsSemaphoreRenderFinished :: Vk.Semaphore
              }

data FrameInFlight = FrameInFlight { fifFrameSynce     :: FrameSync
                                   -- TODO: Move to renderer?
                                   , fifDescriptorPool :: Vk.DescriptorPool
                                   , fifCommandPool    :: Vk.CommandPool
                                   }

data FramesInFlight
  = FramesInFlight { framesInFlightFrameSync      :: TQueue (ReleaseKey, FrameSync)
                   -- TODO: Move to renderer?
                   , framesInFlightDescriptorPool :: TQueue Vk.DescriptorPool
                   , framesInFlightCommandPool    :: TQueue Vk.CommandPool
                   }

type NumFramesInFlight = Word32

acquireDescriptorPool
  :: (MonadMask m, MonadIO m)
  => Vk.Device
  -> TQueue Vk.DescriptorPool
  -> (Vk.DescriptorPool -> m b)
  -> m b
acquireDescriptorPool device que f = do
  bracket (liftIO . STM.atomically . STM.readTQueue $ que)
          (liftIO . STM.atomically . STM.writeTQueue que)
          (\descPool ->
             f descPool
               `finally` Vk.resetDescriptorPool device descPool Vk.zero
          )

acquireCommandPool
  :: ( MonadMask m
     , MonadIO m
     )
  => Vk.Device
  -> TQueue Vk.CommandPool
  -> (Vk.CommandPool -> m b)
  -> m b
acquireCommandPool device que f = do
  bracket (liftIO . STM.atomically . STM.readTQueue $ que)
          (liftIO . STM.atomically . STM.writeTQueue que)
          (\cmdPool ->
             f cmdPool
               `finally` Vk.resetCommandPool device cmdPool Vk.zero
          )

acquireFrameSync
  :: ( MonadMask m
     , MonadResource m
     )
  => Vk.Device
  -> TQueue (ReleaseKey, FrameSync)
  -> (FrameSync -> m r)
  -> m r
acquireFrameSync device que f = do
  mask $ \restore -> do
    (relKey, fs) <- liftIO . STM.atomically . STM.readTQueue $ que
    r <- restore (f fs) `onException` do
      -- If we've encountered an exception while using the frame sync, it's very
      -- likely that we've left one of the fences or semaphores in an invalid
      -- state. We shouldn't try to re-use the frame sync, instead we just
      -- create a new frame sync and destroy the old one.
      --
      -- We use uninterruptibleMask_ to make sure that we always are returning a
      -- valid frame sync back to the TQueue.
      uninterruptibleMask_ $ do
        -- NOTE: We are assuming that none of these operations will block for a
        -- long time.
        fs' <- allocateAcquire $ withFrameSync device
        release relKey
        liftIO . STM.atomically . STM.writeTQueue que $ fs'
    _ <- liftIO . STM.atomically . STM.writeTQueue que $ (relKey, fs)
    pure r

withNextFrameInFlight
  :: ( MonadMask m
     , MonadResource m
     )
  => Vk.Device
  -> FramesInFlight
  -> (FrameInFlight -> m r)
  -> m r
withNextFrameInFlight device frames f =
  acquireFrameSync device (framesInFlightFrameSync frames) $ \fs ->
  acquireDescriptorPool device (framesInFlightDescriptorPool frames) $ \descPool ->
  acquireCommandPool device (framesInFlightCommandPool frames) $ \cmdPool ->
    f (FrameInFlight fs descPool cmdPool)

withFramesInFlight
  :: MonadResource m
  => Vk.Device
  -> QueueFamilies
  -> NumFramesInFlight
  -> m FramesInFlight
withFramesInFlight device queueFamilies numFramesInFlight = do
  frameSyncQue      <- liftIO newTQueueIO
  descriptorPoolQue <- liftIO newTQueueIO
  cmdPoolQue        <- liftIO newTQueueIO

  replicateM_ (fromIntegral numFramesInFlight) $ do
    fs <- allocateAcquire $ withFrameSync device
    -- TODO: We just hard-code some large sizes for the pool. In the future we
    -- might analyse the render graph to allocate the right number of descriptor
    -- sets, OR we could use a free list of descriptor pools.
    let
      maxSets = 1024
      maxUniformBuffers = 8192
      maxImageSamplers = 8192
    (_, descPool) <-
      allocateAcquire $ withVkDescriptorPool
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
    (_, cmdPool) <-
      allocateAcquire $ withVkCommandPool
        device
        (NE.head . graphicsQueueFamilies $ queueFamilies)
    liftIO $ STM.atomically $ do
      STM.writeTQueue frameSyncQue fs
      STM.writeTQueue descriptorPoolQue descPool
      STM.writeTQueue cmdPoolQue cmdPool

  pure $ FramesInFlight frameSyncQue descriptorPoolQue cmdPoolQue

withFrameSync
  :: Vk.Device
  -> Acquire FrameSync
withFrameSync logicalDevice = do
  let
    -- Create the fence pre-signalled so the first time we wait on the fence in
    -- drawing, we don't wait forever.
    fenceInfo = Vk.FenceCreateInfo () Vk.FENCE_CREATE_SIGNALED_BIT
    semaphoreInfo = Vk.SemaphoreCreateInfo () Vk.zero
  inFlightFence <- withFence logicalDevice fenceInfo
  renderFinishedSem <- withSemaphore logicalDevice semaphoreInfo
  imageAvailableSem <- withSemaphore logicalDevice semaphoreInfo
  pure $ FrameSync inFlightFence imageAvailableSem renderFinishedSem
