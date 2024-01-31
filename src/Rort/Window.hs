{-# LANGUAGE ScopedTypeVariables #-}
module Rort.Window ( Window
                   , withWindow
                   , withSurface
                   , getRequiredExtensions
                   ) where

import qualified Graphics.UI.GLFW as GLFW
import Numeric.Natural (Natural)
import qualified Rort.Window.GLFW as GLFW
import Data.Text (Text)
import Data.Bifunctor (bimap)
import Data.Int (Int32)
import qualified Vulkan as Vk
import Control.Monad.Trans.Resource (MonadResource)
import qualified Control.Monad.Trans.Resource as ResourceT
import Foreign (alloca, nullPtr, peek)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as BSC
import Foreign.C (peekCString)

data Window = WindowGLFW GLFW.Window

withWindow
  :: Natural
  -> Natural
  -> Text
  -> (Window -> IO ())
  -> IO ()
withWindow width height title f =
  GLFW.withWindow width height title (\win -> f (WindowGLFW win))

getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize (WindowGLFW win) = do
  bimap fromIntegral fromIntegral
    <$> GLFW.getFramebufferSize win

withSurface
  :: MonadResource m
  => Vk.Instance
  -> Window
  -> m Vk.SurfaceKHR
withSurface vkInst (WindowGLFW win) =
  let
    acquire = alloca $ \ptr -> do
      s <- GLFW.createWindowSurface (Vk.instanceHandle vkInst) win nullPtr ptr
      case s of
        (0 :: Int32) -> peek ptr
        res -> error $
          "Failed to create window surface, exit code was: '" <> show res <> "'"
    release surface = Vk.destroySurfaceKHR vkInst surface Nothing
  in
    snd <$> ResourceT.allocate acquire release

getRequiredExtensions :: Window -> IO (Vector BSC.ByteString)
getRequiredExtensions (WindowGLFW _) = do
  rawExts <- GLFW.getRequiredInstanceExtensions
  exts <- traverse (fmap BSC.pack . peekCString) rawExts
  pure $ Vector.fromList exts
