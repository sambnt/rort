{-# LANGUAGE OverloadedRecordDot #-}

module Rort.Render.Pool where

import Control.Concurrent.STM (TArray, STM, TQueue)
import qualified Control.Concurrent.STM as STM
import Data.Word (Word32)
import qualified Data.Array.MArray as Array
import qualified Data.Array.Base as Array
import Control.Monad (forM_)

data Pool a = Pool { elems :: TArray Word32 a
                   , freeIxs :: TQueue Word32
                   }

newtype Handle a = Handle Word32
  deriving Show

newPool :: Word32 -> STM (Pool a)
newPool sz = do
  elemArray <- Array.newArray_ (0, sz - 1)
  freeIxQue <- STM.newTQueue
  forM_ [0..(sz-1)] $ STM.writeTQueue freeIxQue
  pure $ Pool elemArray freeIxQue

addItem :: Pool a -> a -> STM (Handle a)
addItem pool a = do
  mFreeIx <- STM.tryReadTQueue pool.freeIxs
  case mFreeIx of
    Nothing -> error "Ran out of space in pool"
    Just freeIx -> do
      Array.unsafeWrite pool.elems (fromIntegral freeIx) a
      pure $ Handle freeIx

getItem :: Pool a -> Handle a -> STM a
getItem pool (Handle ix) = Array.unsafeRead pool.elems (fromIntegral ix)

putItem :: Pool a -> Handle a -> a -> STM ()
putItem pool (Handle ix) = Array.unsafeWrite pool.elems (fromIntegral ix)
