{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Rort.Util.Defer ( Deferred
                       , eval
                       , evalM
                       , unsafeEval
                       , getSeed
                       , defer
                       ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent (MVar)
import UnliftIO (modifyMVar, MonadUnliftIO, newMVar)

data Deferred a b where
  Deferred :: a -> MVar (Maybe b) -> Deferred a b
  -- TODO: What about extra (a -> b) parameter

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

bulkEvalM :: MonadUnliftIO m => [Deferred a b] -> ([a] -> m [b]) -> m ()
bulkEvalM = undefined

unsafeEval :: MonadUnliftIO m => Deferred a b -> m b
unsafeEval deferred = evalM deferred (error "Value not evaluated!")

defer :: MonadIO m => a -> m (Deferred a b)
defer a = Deferred a <$> newMVar Nothing

getSeed :: Deferred a b -> a
getSeed (Deferred a _) = a
