{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Rort.Util.Defer ( Deferred
                       , eval
                       , evalM
                       ) where

import Control.Concurrent.STM (TQueue, TMVar, tryReadTMVar, putTMVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (($>))
import Control.Concurrent (MVar)
import UnliftIO (modifyMVar, MonadUnliftIO, newMVar)

-- data Defer a b = Defer { evaluated :: TQueue (TMVar b)
--                        }

data Deferred a b where
  Deferred :: a -> MVar (Maybe b) -> Deferred a b

eval :: MonadUnliftIO m => Deferred a b -> (a -> b) -> m b
eval deferred f = do
  evalM deferred (pure . f)

evalM :: MonadUnliftIO m => Deferred a b -> (a -> m b) -> m b
evalM (Deferred a bVar) f = do
  modifyMVar bVar $ \case
    Nothing -> do
      b <- f a
      pure (Just b, b)
    Just b ->
      pure (Just b, b)

defer :: MonadIO m => a -> m (Deferred a b)
defer a = do
  Deferred a <$> newMVar Nothing
