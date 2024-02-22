{-# LANGUAGE ScopedTypeVariables #-}
module Rort.Window ( Window
                   , withWindow
                   , withSurface
                   , getRequiredExtensions
                   , getFramebufferSize
                   -- , getWindowEvent
                   , withWindowEvent
                   , closeWindow
                   ) where

import Numeric.Natural (Natural)
import qualified Rort.Window.GLFW as RortGLFW
import Data.Text (Text)
import qualified Vulkan as Vk
import Control.Monad.Trans.Resource (MonadResource)
import Data.Vector (Vector)
import qualified Data.ByteString.Char8 as BSC
import Rort.Window.Types (WindowEvent)

data Window = GLFW RortGLFW.WindowGLFW

withWindow
  :: Natural
  -> Natural
  -> Text
  -> (Window -> IO ())
  -> IO ()
withWindow width height title f =
  RortGLFW.withWindow width height title (f . GLFW)

getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize (GLFW w) = RortGLFW.getFramebufferSize w

withSurface
  :: MonadResource m
  => Vk.Instance
  -> Window
  -> m Vk.SurfaceKHR
withSurface vkInst (GLFW w) = RortGLFW.withSurface vkInst w

getRequiredExtensions :: Window -> IO (Vector BSC.ByteString)
getRequiredExtensions (GLFW w) = RortGLFW.getRequiredExtensions w

closeWindow :: Window -> IO ()
closeWindow (GLFW w) = RortGLFW.closeWindow w

-- getWindowEvent :: Window -> IO (Maybe WindowEvent)
-- getWindowEvent (GLFW w) = RortGLFW.getWindowEvent w

withWindowEvent :: Window -> (Maybe WindowEvent -> IO r) -> IO r
withWindowEvent (GLFW w) = RortGLFW.withWindowEvent w
