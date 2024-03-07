{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Rort.Render.SwapchainImages where

import Control.Concurrent.STM (TMVar)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Monad.Trans.Resource (ReleaseKey, MonadResource, release)
import Rort.Render.Swapchain (Swapchain, withSwapchain, vkImageViews, isSwapchainOutOfDate)
import Data.Word (Word8)
import Rort.Vulkan.Context (VkContext, vkGetFramebufferSize)
import Control.Monad (forM_, unless)
import Control.Concurrent.STM (newTMVarIO)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Acquire (allocateAcquire)
import qualified Data.Vector as Vector
import Control.Exception.Safe (try, throwIO, mask, MonadMask, onException)

data SwapchainImages
  = SwapchainImages { swapchain   :: TMVar (ReleaseKey, Swapchain)
                    , images      :: TQueue SwapchainImage
                    , imagesInUse :: TQueue ()
                    }

data SwapchainImage = SwapchainImage { imageIndex     :: Word8
                                     -- TODO: Index is given by finallyPresent, should that be here? Or should we remove this index?
                                     , imageSwapchain :: Swapchain
                                     -- We copy the Swapchain to each frame so
                                     -- that each thread acquiring a swapchain
                                     -- image doesn't have to contend with other
                                     -- thread to read the TMVar. We know the
                                     -- Swapchain will only change when it's
                                     -- re-created.
                                     }

create :: MonadResource m => VkContext -> m SwapchainImages
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

  imageInUseQue <- liftIO STM.newTQueueIO

  pure $ SwapchainImages swapchainVar imageQue imageInUseQue

mkSwapchainImages :: Swapchain -> [SwapchainImage]
mkSwapchainImages sc =
  foldMap
    (\(ix,_) -> [SwapchainImage ix sc])
    (zip [0..] $ Vector.toList $ vkImageViews sc)

recreateSwapchain :: (MonadMask m, MonadResource m) => VkContext -> SwapchainImages -> (Swapchain -> m ()) -> m ()
recreateSwapchain ctx imgs after = do
  mSwapchain <- liftIO $ STM.atomically $ STM.tryTakeTMVar imgs.swapchain
  case mSwapchain of
    Nothing ->
      -- Someone is already working on recreating the swapchain. Do nothing.
      pure ()
    Just (oldRelKey, oldSc) -> mask $ \restore -> do
      let
        newSwapchainFromOld = do
          -- Wait for all images to be returned
          restore $ liftIO $ STM.atomically $ do
            empty <- STM.isEmptyTQueue imgs.imagesInUse
            unless empty STM.retry
          -- Then create new swapchain and free old swapchain
          framebufferSize <-
            liftIO $ vkGetFramebufferSize ctx
          sc <- allocateAcquire $
            withSwapchain ctx framebufferSize (Just oldSc)
          release oldRelKey
          pure sc
      -- Clear all existing images
      oldImgs <- liftIO $ STM.atomically $ STM.flushTQueue imgs.images
      (newRelKey, newSc) <-
        newSwapchainFromOld
          `onException` liftIO (STM.atomically $ do
                                   forM_ oldImgs $ STM.writeTQueue imgs.images
                                   STM.putTMVar imgs.swapchain (oldRelKey, oldSc)
                               )
      liftIO $ STM.atomically $ do
        STM.putTMVar imgs.swapchain (newRelKey, newSc)
        let newImgs = mkSwapchainImages newSc
        forM_ newImgs $ STM.writeTQueue imgs.images
      restore (after newSc)


withSwapchainImage :: (MonadMask m, MonadIO m) => SwapchainImages -> (SwapchainImage -> m r) -> m r
withSwapchainImage imgs f = mask $ \restore -> do
  -- Acquire a new image and let the SwapchainImages know it's "in use"/"out".
  img <- liftIO $ STM.atomically $ do
    STM.writeTQueue imgs.imagesInUse ()
    STM.readTQueue imgs.images

  result <- try (restore (f img))
  case result of
    Left ex -> do
      liftIO $ STM.atomically $ do
        STM.readTQueue imgs.imagesInUse
        -- Don't return the swapchain image when swapchain is out of date -
        -- it's not usable.
        unless (isSwapchainOutOfDate ex) $
          STM.writeTQueue imgs.images img
      liftIO $ throwIO ex
    Right r -> do
      liftIO $ STM.atomically $ do
        STM.readTQueue imgs.imagesInUse
        STM.writeTQueue imgs.images img
      pure r
