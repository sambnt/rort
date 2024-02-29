{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Rort.Render.FRP where

import Data.MonadicStreamFunction (MSF, arrM, embed, constM, (>>>), feedback, arr, morphS, MStream, MSink)
import FRP.BearRiver (Event (..), ClockInfo, reactimate, SF, hold, event, dup)
import Rort.Render.Swapchain (Swapchain, withSwapchain)
import Control.Concurrent.STM (TMVar, readTMVar, takeTMVar, putTMVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.Resource (ReleaseKey, MonadResource, runResourceT, release)
import Control.Monad.Reader (ReaderT, ask, MonadReader, runReaderT, mapReaderT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Rort.Vulkan.Context (VkContext, vkGetFramebufferSize, VkSettings (..), withVkContext)
import Data.Acquire (allocateAcquire)
import Control.Exception.Safe (MonadMask(mask), onException)
import qualified Vulkan as Vk
import qualified Data.Vector as Vector
import Rort.Window (getRequiredExtensions, withWindow)
import Rort.Render.Types (DrawCall (PrimitiveDraw), Draw (..), DrawCallPrimitive (..), RenderPass)
import Data.Functor.Contravariant (contramap)
import Data.MonadicStreamFunction.ReactHandle (reactInit, react)
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Functor (void)

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

-- swapchainSF'
--   :: (MonadIO m, Monad m)
--   => Renderer
--   -> MSF m (Event Swapchain) Swapchain
-- swapchainSF' r = proc ev -> do
--   initialSwapchain <- constM (liftIO $ STM.atomically $ snd <$> readTMVar r.swapchainVar) -< ()
--   hold' initialSwapchain -< ev

-- hold' :: Monad m => c -> MSF m (Event c) c
-- hold' a = feedback a $ arr $ \(e, a') ->
--   dup (event a' id e)

-- feedbackM :: Monad m => m c -> MSF m (a, c) (b, c) -> MSF m a b
-- feedbackM mc sf = MSF $ \a -> do
--   c <- mc
--   ((b', c'), sf') <- unMSF sf (a, c)
--   pure (b', feedbackM (pure c') sf')

-- renderPassSF :: MSF m (Swapchain, RenderPassInfo) RenderPass
-- renderPassSF = proc sc -> do
--   _

-- | Run an MSF on an input sample step by step, using an IORef to store the
-- continuation.
pushReactimate :: MSF IO a b -> IO (a -> IO b)
pushReactimate msf = do
  msfRef <- newIORef msf
  return $ \a -> do
              msf' <- readIORef msfRef
              (b, msf'') <- unMSF msf' a
              writeIORef msfRef msf''
              return b

-- | Run one step of an MSF on () streams, internally storing the
-- continuation.
pushReactimate_ :: MSF IO () () -> IO (IO ())
pushReactimate_ msf = do
  f <- pushReactimate msf
  return (void (f ()))

type ReactiveValueProducer m a = (MStream m a, m () -> m ())
type ReactiveValueConsumer m a = MSink   m a
type ReactiveValue m a = (MStream m a, MSink m a, m () -> m ())

liftRW2 :: Monad m => (a -> b, b -> a) -> ReactiveValue m a -> ReactiveValue m b
liftRW2 (f, f') (sg, sk, h) = (sg >>> arr f, arr f' >>> sk, h)

(=:=) :: (Show a, Eq a) => ReactiveValue IO a -> ReactiveValue IO a -> IO ()
(sg1,sk1,h1) =:= (sg2, sk2, h2) = do
  (sg1,h1) =:> sk2
  (sg2,h2) =:> sk1

(=:>) :: (Show a, Eq a) => ReactiveValueProducer IO a -> ReactiveValueConsumer IO a -> IO ()
(sg, h) =:> sk = h =<< pushReactimate_ (sg >>> sk)

swapchainSF
  :: ( MonadResource m
     , MonadMask m
     )
  => VkContext -> Renderer -> MSF m (Event SwapchainEvent) Swapchain
  -- TODO: Should this output (Event Swapchain)? No need to re-read TMVar unless event occurred
swapchainSF ctx r = do
  arrM $ \ev -> do
    case ev of
      NoEvent -> do
        liftIO $ putStrLn "Reading swapchain"
        liftIO $ STM.atomically $ snd <$> readTMVar r.swapchainVar
        -- pure NoEvent
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

        y :: (MonadResource m, MonadMask m) => MSF m () ()
        y = feedback NoEvent (proc ((), ev) -> do
                                 sc <- swapchainSF ctx r -< ev
                                 draws <- renderLoop -< ()
                                 drawSF -< draws
                             )
      -- embed (swapchainSF ctx) [NoEvent, Event SwapchainOutOfDate, NoEvent]
      embed (y) [(), (), ()]

      pure ()
