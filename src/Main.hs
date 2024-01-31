{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(PipelineBindPoint))
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as BSC
import Foreign.C (peekCString)
import Numeric.Natural (Natural)

main = do
  withWindow 800 600 "Hello World" $ \win -> do
    rawExts <- GLFW.getRequiredInstanceExtensions
    glfwExts <- Vector.fromList <$> traverse (fmap BSC.pack . peekCString) rawExts

    let createInfo = Vk.InstanceCreateInfo
          ()                                                 -- Chain
          (Vk.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR) -- Flags (Need this flag for MacOS: https://github.com/KhronosGroup/MoltenVK#using-the-vulkan-sdk)
          (Just $ Vk.ApplicationInfo
             (Just "Dross")
             (Vk.MAKE_API_VERSION 1 0 0)
             (Just "No engine")
             (Vk.MAKE_API_VERSION 1 0 0)
             (Vk.MAKE_API_VERSION 1 0 0)
          )
          (Vector.fromList []) -- enabledLayerNames
          -- (Vector.fromList [ "VK_LAYER_KHRONOS_validation" ]) -- enabledLayerNames
          -- (Vector.fromList [ "VK_LAYER_RENDERDOC_Capture" ]) -- enabledLayerNames
          ( glfwExts
            <> Vector.fromList [ "VK_KHR_portability_enumeration" ] -- Required for MacOS (https://github.com/KhronosGroup/MoltenVK#using-the-vulkan-sdk)
          )

    props <- Vk.enumerateInstanceLayerProperties
    putStrLn $ "props: " <> show props

    inst <- Vk.createInstance createInfo Nothing
    putStrLn $ "inst: " <> show inst

    threadDelay 1000000

withWindow :: Natural -> Natural -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f =
  bracket
    GLFW.init
    (const GLFW.terminate)
    (\succeeded -> when succeeded $ do
      -- Don't use an OpenGL API
      GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
      -- Resizing takes extra work
      GLFW.windowHint $ GLFW.WindowHint'Resizable True
      bracket
        (GLFW.createWindow (fromIntegral width) (fromIntegral height) title Nothing Nothing)
        (\case
            (Just win) -> GLFW.destroyWindow win
            Nothing -> pure ()
        )
        (\mWin ->
          case mWin of
            Nothing -> do
              err <- GLFW.getError
              print err
            (Just win) -> do
              GLFW.setErrorCallback $ Just simpleErrorCallback
              f win
        )
    )
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]
