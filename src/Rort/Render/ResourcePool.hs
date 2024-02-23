{-# LANGUAGE OverloadedRecordDot #-}

module Rort.Render.ResourcePool where

import Rort.Render.Pool (Pool, newPool, addItem, Handle, getItem, putItem)
import Data.Acquire (Acquire, with)
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Data.Word (Word32)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, MonadUnliftIO)

-- data Resource a b = ToBeAcquired (Acquire a)
--                   | Acquired b

data Resource a b = ToBeAcquired (Acquire a)
                  | Acquired b

type ResourceHandle a b = Handle (Resource a b)

data ResourcePool a b = ResourcePool { pool :: Pool (Resource a b) }

newResourcePool :: Word32 -> STM (ResourcePool a b)
newResourcePool maxSz = ResourcePool <$> newPool maxSz

addResource :: ResourcePool a b -> Acquire a -> STM (ResourceHandle a b)
addResource rs = addItem rs.pool . ToBeAcquired

loadResource
  :: MonadUnliftIO
  => ResourcePool a b
  -> (a -> m b)
  -> ResourceHandle a b
  -> m b
loadResource rs f h = do
  r <- liftIO $ STM.atomically $ getItem rs.pool h
  case r of
    ToBeAcquired acquireA -> do
      b <- with acquireA f
      liftIO $ STM.atomically $ putItem rs.pool h (Acquired b)
      pure b
    Acquired b ->
      pure b
