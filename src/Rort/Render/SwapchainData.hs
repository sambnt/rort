{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rort.Render.SwapchainData where

import Rort.Render.Swapchain (Swapchain, vkImageViews, withSwapchain, SwapchainOutOfDate (..), isSwapchainOutOfDate)
import Control.Concurrent.STM (TMVar, TQueue, TVar, newTMVarIO, newTVarIO, isEmptyTQueue, retry, flushTQueue)
import qualified Control.Concurrent.STM as STM
import Data.Word (Word8)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, MonadResource, release)
import Rort.Vulkan.Context (VkContext, vkGetFramebufferSize)
import qualified Data.Vector as Vector
import Control.Monad (forM, forM_, unless)
import Data.Acquire (allocateAcquire)
import Control.Exception.Safe (MonadMask(mask), try, onException, throwIO, SomeException, mask_)

data SwapchainData = SwapchainData { swapchain    :: TMVar (ReleaseKey, Swapchain)
                                   -- TODO: TVar (ReleaseKey, Swapchain)?
                                   , images       :: TQueue SwapchainImage
                                   , imagesOut    :: TQueue ()
                                   }

data SwapchainImage
  = SwapchainImage { imageIx :: Word8
                   , imageSwapchain :: Swapchain
                   -- We copy the Swapchain to each frame so that each thread
                   -- acquiring a frame doesn't also have to read the TMVar. We
                   -- know the Swapchain will only change when it is re-created.
                   }

create :: MonadResource m => VkContext -> m SwapchainData
create ctx = do
  framebufferSize <-
    liftIO $ vkGetFramebufferSize ctx
  (relKey, sc) <-
    allocateAcquire $
      withSwapchain ctx framebufferSize Nothing

  swapchainVar <- liftIO $ newTMVarIO (relKey, sc)

  let imgs = mkSwapchainImages sc
  imageQue <- liftIO $ STM.atomically $ do
    imageQue <- STM.newTQueue
    forM_ imgs $ STM.writeTQueue imageQue
    pure imageQue
  imageOutQue <- liftIO STM.newTQueueIO

  pure $ SwapchainData swapchainVar imageQue imageOutQue

mkSwapchainImages :: Swapchain -> [SwapchainImage]
mkSwapchainImages sc =
  foldMap
    (\(ix,_) -> [SwapchainImage ix sc])
    (zip [0..] $ Vector.toList $ vkImageViews sc)

recreateSwapchain :: (MonadMask m, MonadResource m) => VkContext -> SwapchainData -> (Swapchain -> m ()) -> m ()
recreateSwapchain ctx dat after = do
  mSwapchain <- liftIO $ STM.atomically $ STM.tryTakeTMVar dat.swapchain
  case mSwapchain of
    Nothing ->
      -- Someone is already working on it...
      pure ()
    Just (oldRelKey, oldSc) -> do
      mask $ \restore -> do
        let
          newSwapchainFromOld = do
            -- Clear all existing images
            _ <- liftIO $ STM.atomically $ flushTQueue dat.images
            -- Wait for all images to be returned
            restore $ liftIO $ STM.atomically $ do
              empty <- isEmptyTQueue dat.imagesOut
              unless empty retry
            -- Then create new swapchain and free old swapchain
            framebufferSize <-
              liftIO $ vkGetFramebufferSize ctx
            sc <- allocateAcquire $
              withSwapchain ctx framebufferSize (Just oldSc)
            release oldRelKey
            pure sc
        (newRelKey, newSc) <-
          newSwapchainFromOld
            `onException`
              liftIO (STM.atomically $
                       STM.putTMVar dat.swapchain (oldRelKey, oldSc)
                     )
        liftIO $ STM.atomically $ do
          STM.putTMVar dat.swapchain (newRelKey, newSc)
          let imgs = mkSwapchainImages newSc
          forM_ imgs $ STM.writeTQueue dat.images
        after newSc

withSwapchainImage :: (MonadMask m, MonadIO m) => SwapchainData -> (SwapchainImage -> m r) -> m r
withSwapchainImage dat f = mask $ \restore -> do
  -- Acquire new image and let the Swapchain know it's "out".
  img <- liftIO $ STM.atomically $ do
    img <- STM.readTQueue dat.images
    STM.writeTQueue dat.imagesOut ()
    pure img

  result <- try (restore (f img))
  case result of
    Left ex -> do
      liftIO $ STM.atomically $ do
        -- Don't return the swapchain image when swapchain out of date - it's
        -- not usable.
        unless (isSwapchainOutOfDate ex) $
          STM.writeTQueue dat.images img
        STM.readTQueue dat.imagesOut
      liftIO $ throwIO ex
    Right r -> do
      liftIO $ STM.atomically $ do
        STM.writeTQueue dat.images img
        STM.readTQueue dat.imagesOut
      pure r
