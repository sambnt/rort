{-# LANGUAGE LambdaCase #-}

module Rort.Window.GLFW where

import Numeric.Natural (Natural)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad.Catch (bracket)
import Control.Monad (when)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as BSC
import Foreign.C (peekCString)

withWindow
  :: Natural
  -> Natural
  -> Text
  -> (GLFW.Window -> IO ())
  -> IO ()
withWindow width height title f = do
  let
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

  GLFW.setErrorCallback $ Just simpleErrorCallback

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
            GLFW.setErrorCallback $ Just simpleErrorCallback
            f win


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
