{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rort.Window.GLFW where

import Numeric.Natural (Natural)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Exception.Safe (bracket, mask, onException)
import Control.Monad (when)
import Control.Concurrent.STM (TQueue)
import qualified Control.Concurrent.STM as STM
import Rort.Window.Types (WindowEvent (..))
import Data.Vector (Vector)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int32)
import Foreign (alloca, peek)
import Foreign.C (peekCString)
import Foreign.Ptr (nullPtr)
import Data.Bifunctor (bimap)
import Data.Acquire (Acquire, mkAcquire)

data WindowGLFW = WindowGLFW GLFW.Window (TQueue WindowEvent)

withWindow
  :: Natural
  -> Natural
  -> Text
  -> (WindowGLFW -> IO ())
  -> IO ()
withWindow width height title f = do
  eventsQue <- STM.newTQueueIO

  withGLFW $ \r -> do
    -- Don't use the OpenGL API
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    -- Allow resizing of the window
    GLFW.windowHint $ GLFW.WindowHint'Resizable True

    when r $ do
      withGLFWWindow width height title $ \mWin -> do
        case mWin of
          Nothing -> do
            err <- GLFW.getError
            print err
          (Just win) -> do
            GLFW.setErrorCallback $ Just $ \err str -> do
              queue eventsQue
                $ WindowError $ T.pack $ show err <> ": " <> str
            GLFW.setWindowCloseCallback win $ Just $ \_w -> do
              queue eventsQue WindowClose
            GLFW.setFramebufferSizeCallback win $ Just $ \_w x y -> do
              queue eventsQue $ WindowResize x y

            f $ WindowGLFW win eventsQue


queue :: TQueue a -> a -> IO ()
queue que = STM.atomically . STM.writeTQueue que

withGLFW
  :: (Bool -> IO ())
  -- ^ Bool represents whether GLFW successfully initialized or not.
  -> IO ()
withGLFW =
  bracket
    GLFW.init
    (const GLFW.terminate)

withGLFWWindow
  :: Natural -> Natural -> Text -> (Maybe GLFW.Window -> IO ()) -> IO ()
withGLFWWindow width height title =
  bracket
    (GLFW.createWindow (fromIntegral width) (fromIntegral height) (T.unpack title) Nothing Nothing)
    (\case
        (Just win) -> GLFW.destroyWindow win
        Nothing    -> pure ()
    )

closeWindow :: WindowGLFW -> IO ()
closeWindow (WindowGLFW win _que) = GLFW.setWindowShouldClose win True

getWindowEvent :: WindowGLFW -> IO (Maybe WindowEvent)
getWindowEvent (WindowGLFW _win que) = do
  GLFW.pollEvents
  STM.atomically $ STM.tryReadTQueue que

withWindowEvent :: WindowGLFW -> (Maybe WindowEvent -> IO r) -> IO r
withWindowEvent (WindowGLFW _win que) f = do
  GLFW.pollEvents
  mask $ \restore -> do
    mEv <- STM.atomically $ STM.tryReadTQueue que
    restore (f mEv)
      `onException` case mEv of
                        Nothing -> pure ()
                        Just ev -> STM.atomically $ STM.writeTQueue que ev


getRequiredExtensions :: WindowGLFW -> IO (Vector BSC.ByteString)
getRequiredExtensions (WindowGLFW _ _) = do
  rawExts <- GLFW.getRequiredInstanceExtensions
  exts <- traverse (fmap BSC.pack . peekCString) rawExts
  pure $ Vector.fromList exts

withVkSurface
  :: Vk.Instance
  -> WindowGLFW
  -> Acquire Vk.SurfaceKHR
withVkSurface vkInst (WindowGLFW win _) =
  let
    acquire = alloca $ \ptr -> do
      s <- GLFW.createWindowSurface (Vk.instanceHandle vkInst) win nullPtr ptr
      case s of
        (0 :: Int32) -> peek ptr
        res -> error $
          "Failed to create window surface, exit code was: '" <> show res <> "'"
    release surface = Vk.destroySurfaceKHR vkInst surface Nothing
  in
    mkAcquire acquire release

getFramebufferSize :: WindowGLFW -> IO (Int, Int)
getFramebufferSize (WindowGLFW w _) =
  bimap fromIntegral fromIntegral
    <$> GLFW.getFramebufferSize w
