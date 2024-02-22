{-# LANGUAGE ScopedTypeVariables #-}
module Test.Rort.Util.Resource where

import Control.Exception.Safe (SomeException, throwIO)
import Control.Monad.Catch (catch, onException, mask, bracket, MonadMask (uninterruptibleMask))
import Control.Concurrent (threadDelay, killThread, forkIO)
import Rort.Util.Resource (fromResourceT, Resource)
import qualified Rort.Util.Resource as Resource
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import qualified Control.Monad.Trans.Resource as ResourceT
import Control.Monad.IO.Class (liftIO)
import System.IO (openFile, IOMode (ReadMode), hClose, Handle, hGetLine)
import Control.Monad.Cont (ContT(ContT), runContT)

acquire = putStrLn "Acquired resource!"
free = putStrLn "Freed resource!"











test1 = do
  acquire
  putStrLn "Doing some work..."
  error "exception!"
  free






withResource1 someWork = do
  acquire
  someWork `catch` (\e -> do
                       _ <- free
                       throwIO (e :: SomeException)
                   )
  free





withResource2 someWork = do
  acquire
  someWork `onException` free
  free





asyncException with someWork = do
  tid <- forkIO (with someWork)
  threadDelay 10000
  killThread tid



withResource3 someWork = do
  acquire
  threadDelay 1000000
  someWork `onException` free
  free




withResource4 someWork = mask $ \restore -> do
  acquire
  someWork `onException` free
  free




withResource5 someWork = mask $ \restore -> do
  acquire
  restore (someWork) `onException` free
  free


-- bracket
--   (openFile "filename1" ReadMode)
--   (hClose)
--   (\fileHandle1 ->
--      bracket
--        (openFile "filename2" ReadMode)
--        hClose
--        (\fileHandle2 ->
--           _someWork
--        )
--   )

-- ContT ((a -> m r) -> m r)
withFile
  :: FilePath
  -> (Handle -> IO r)
  -> IO r
withFile fileName someWork =
  bracket
    (openFile fileName ReadMode)
    hClose
    someWork

withFileCont
  :: FilePath -> ContT r IO Handle
withFileCont fileName =
  ContT $ withFile fileName

main :: IO ()
main = flip runContT pure $ do
  -- withFile "README.md" $ \readme -> do
  --   withFile "notes.md" $ \notes -> do
  --     firstLine <- hGetLine readme
  --     print firstLine
  notesH <- withFileCont "notes.md"
  readmeH <- withFileCont "README.md"
  firstLineR <- liftIO $ hGetLine readmeH
  liftIO $ print firstLineR
  firstLineN <- liftIO $ hGetLine notesH
  liftIO $ print firstLineN




-- bracket acquireResource freeResource someWork = mask $ \restore -> do
--   acquireResource
--   restore someWork `onException` freeResource
--   freeResource

data MyData = MyData () ()
  deriving Show

testResource :: MonadResource m => m (Resource MyData)
testResource = do
  res1 <- Resource.allocate
    (putStrLn "Acquired Resource 1!")
    (const $ putStrLn "Freed Resource 1!")
  res2 <- Resource.allocate
    (putStrLn "Acquired Resource 2!")
    (const $ putStrLn "Freed Resource 2!")
  pure $ MyData <$> res1 <*> res2

eg :: IO ()
eg = ResourceT.runResourceT $ do
  (myData :: Resource MyData) <- testResource
  liftIO $ print $ Resource.get myData
  liftIO $ Resource.free myData
  (myData2 :: Resource MyData) <- testResource
  liftIO $ print $ Resource.get myData2



-- withResource5 someWork =
--   bracket acquire (const free) $ \_ ->
--     bracket (threadDelay 1000000) (const $ pure ()) someWork
