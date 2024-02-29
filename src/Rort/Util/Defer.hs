{-# LANGUAGE GADTs #-}

module Rort.Util.Defer ( Deferred
                       , defer
                       , eval
                       , evalEmpty
                       , unsafeGet
                       , getSeed
                       ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.STM (TMVar, takeTMVar, newTMVarIO, putTMVar, readTMVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe (mask, onException, MonadMask)

data Deferred a b where
  Deferred :: a -> TMVar (Maybe b) -> Deferred a b

defer :: MonadIO m => a -> m (Deferred a b)
defer a = Deferred a <$> liftIO (newTMVarIO Nothing)

eval :: (MonadMask m, MonadIO m) => Deferred a b -> (a -> Maybe b -> m b) -> m b
eval (Deferred a bVar) f = mask $ \restore -> do
  mb <- liftIO . STM.atomically $ takeTMVar bVar
  b <- restore (f a mb) `onException` (liftIO . STM.atomically $ putTMVar bVar mb)
  liftIO . STM.atomically $ putTMVar bVar (Just b)
  pure b

getSeed :: Deferred a b -> a
getSeed (Deferred a _) = a

-- | Only eval if value is empty.
evalEmpty
  :: ( MonadIO m
     , MonadMask m
     )
  => Deferred a b
  -> (a -> m b)
  -> m b
evalEmpty d@(Deferred _ bVar) f = do
  mb <- liftIO . STM.atomically $ readTMVar bVar
  case mb of
    Nothing -> eval d (\a _ -> f a)
    Just b -> pure b

unsafeGet :: MonadIO m => Deferred a b -> m b
unsafeGet (Deferred _ bVar) = do
  mb <- liftIO . STM.atomically $ readTMVar bVar
  case mb of
    Nothing -> error "Deferred value not yet evaluated"
    Just b  -> pure b
