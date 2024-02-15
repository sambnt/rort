module Rort.Render.FramesInFlight where

import Control.Concurrent.STM (TQueue, newTQueueIO)
import qualified Control.Concurrent.STM as STM
import qualified Vulkan as Vk
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Word (Word32)
import Control.Monad (replicateM_)
import qualified Vulkan.Zero as Vk
import Rort.Vulkan (withFence, withSemaphore)
import Rort.Util.Resource (Resource)
import qualified Rort.Util.Resource as Resource

data FrameSync
  = FrameSync { fsFenceInFlight           :: Vk.Fence
              , fsSemaphoreImageAvailable :: Vk.Semaphore
              , fsSemaphoreRenderFinished :: Vk.Semaphore
              }

data FramesInFlight
  = FramesInFlight { framesInFlightFrameSync :: TQueue FrameSync
                   }

type NumFramesInFlight = Word32

withNextFrameInFlight
  :: ( MonadMask m
     , MonadIO m
     )
  => FramesInFlight
  -> (FrameSync -> m r)
  -> m r
withNextFrameInFlight s =
  bracket (liftIO . STM.atomically . STM.readTQueue $ framesInFlightFrameSync s)
          (liftIO . STM.atomically . STM.writeTQueue (framesInFlightFrameSync s))

withFramesInFlight
  :: MonadResource m
  => Vk.Device
  -> NumFramesInFlight
  -> m FramesInFlight
withFramesInFlight device numFramesInFlight = do
  framesQue <- liftIO newTQueueIO
  replicateM_ (fromIntegral numFramesInFlight) $ do
    fs <- Resource.get <$> withFrameSync device
    liftIO $ STM.atomically $ STM.writeTQueue framesQue fs

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
