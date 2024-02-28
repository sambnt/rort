{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Rort.Render.FRP where

import Data.MonadicStreamFunction (MSF, arrM, embed, constM, (>>>), feedback, arr)
import FRP.BearRiver (Event (..), ClockInfo)
import Rort.Render.Swapchain (Swapchain, withSwapchain)
import Control.Concurrent.STM (TMVar, readTMVar, takeTMVar, putTMVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.Resource (ReleaseKey, MonadResource, runResourceT, release)
import Control.Monad.Reader (ReaderT, ask, MonadReader, runReaderT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Rort.Vulkan.Context (VkContext, vkGetFramebufferSize, VkSettings (..), withVkContext)
import Data.Acquire (allocateAcquire)
import Control.Exception.Safe (MonadMask(mask), onException)
import qualified Vulkan as Vk
import qualified Data.Vector as Vector
import Rort.Window (getRequiredExtensions, withWindow)
import Rort.Render.Types (DrawCall (PrimitiveDraw), Draw (..), DrawCallPrimitive (..))
import Data.Functor.Contravariant (contramap)

data SwapchainEvent = SwapchainOutOfDate

data Renderer
  = Renderer { swapchainVar :: TMVar (ReleaseKey, Swapchain)
             }

createRenderer ctx numFramesInFlight = do
  swapchain <- createSwapchain ctx Nothing

  var <- liftIO $ STM.newTMVarIO swapchain

  pure $ Renderer var

createSwapchain
  :: MonadResource m
  => VkContext
  -> Maybe (ReleaseKey, Swapchain)
  -> m (ReleaseKey, Swapchain)
createSwapchain ctx mOldSwapchain = do
  framebufferSize <- liftIO $ vkGetFramebufferSize ctx
  allocateAcquire $ do
    sc <- withSwapchain ctx framebufferSize (snd <$> mOldSwapchain)
    case mOldSwapchain of
      Nothing ->
        pure ()
      Just (relKey, _) -> do
        liftIO $ putStrLn "Freeing old swapchain"
        release relKey
    pure sc

swapchainSF
  :: ( MonadReader Renderer m
     , MonadResource m
     , MonadMask m
     )
  => VkContext -> MSF m (Event SwapchainEvent) Swapchain
swapchainSF ctx = do
  arrM $ \ev -> do
    r <- ask
    case ev of
      NoEvent -> do
        liftIO $ putStrLn "Reading swapchain"
        liftIO $ STM.atomically $ snd <$> readTMVar r.swapchainVar
      (Event SwapchainOutOfDate) -> do
        liftIO $ putStrLn "Swapchain OOD"
        mask $ \restore -> do
          oldSwapchain <-
            liftIO $ STM.atomically $
              takeTMVar r.swapchainVar
          (newRelKey, newSwapchain) <-
            restore (createSwapchain ctx (Just oldSwapchain)) `onException` do
              liftIO $ STM.atomically $
                putTMVar r.swapchainVar oldSwapchain
          liftIO $ STM.atomically $
            putTMVar r.swapchainVar (newRelKey, newSwapchain)
          pure newSwapchain

drawSF :: MonadIO m => MSF m [Draw] ((), Event SwapchainEvent)
drawSF = constM $ pure ((), Event SwapchainOutOfDate)

main = do
  let
    width = 800
    height = 600

  withWindow width height "Example: FRP" $ \win -> do
    windowExts <- getRequiredExtensions win

    runResourceT $ do
      let
        cfg = VkSettings { requiredExtensions =
                             windowExts <> Vector.fromList []
                         , requiredValidationLayers =
                             Vector.fromList [ "VK_LAYER_KHRONOS_validation" ]
                         , applicationInfo =
                             Vk.ApplicationInfo
                               (Just "Example: Buffer")  -- application name
                               (Vk.MAKE_API_VERSION 1 0 0) -- application version
                               (Just "No engine")          -- engine name
                               (Vk.MAKE_API_VERSION 1 0 0) -- engine version
                               (Vk.MAKE_API_VERSION 1 0 0) -- Vulkan API version (patch version ignored)
                         }

      (_, ctx) <- allocateAcquire $ withVkContext cfg win

      let numFramesInFlight = 2
      r <- createRenderer ctx numFramesInFlight

      flip runReaderT r $ do
        let
          renderLoop :: Monad m => MSF m () [Draw]
          renderLoop = constM $ do
            pure [ Draw { drawSubpass = undefined
                        , drawVertexBuffers = []
                        , drawIndexBuffer = Nothing
                        , drawCall = PrimitiveDraw $ DrawCallPrimitive
                          { drawCallPrimitiveFirstVertex = 0
                          , drawCallPrimitiveFirstInstance = 0
                          , drawCallPrimitiveInstanceCount = 1
                          , drawCallPrimitiveVertexCount = 3
                          }
                        }
                 ]
          -- x = renderLoop >>> drawSF

          y :: (MonadReader Renderer m, MonadResource m, MonadMask m) => MSF m () ()
          y = feedback NoEvent (do
                                   arr snd
                                     >>> swapchainSF ctx
                                     >>> pure ()
                                     >>> renderLoop
                                     >>> drawSF
                               )
        -- embed (swapchainSF ctx) [NoEvent, Event SwapchainOutOfDate, NoEvent]
        embed (y) [(), (), ()]
      pure ()
