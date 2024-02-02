module Rort.Render.Renderer ( Renderer
                            , Handle
                            , create
                            , mkPipeline
                            , mkShader
                            ) where

import Control.Monad.Trans.Resource (MonadResource)
import Rort.Vulkan.Context (VkContext, vkGetFramebufferSize, vkDevice, vkPresentationQueue)
import Rort.Render.Types (PipelineInfo, Pipeline, ShaderInfo, Shader, Handle(..), renderPassAttachments, pipelineRenderPassLayout, AttachmentInfo (SwapchainAttachment))
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM
import qualified Rort.Util.Resource as Resource
import Data.Word (Word32)
import qualified Vulkan.Extensions.VK_KHR_surface as VkFormat
import Rort.Util.Resource (Resource)
import Rort.Render.Swapchain (Swapchain, withSwapchain, withNextFrameInFlight, throwSwapchainOutOfDate, vkSwapchain, fsSemaphoreImageAvailable, fsFenceInFlight, fsSemaphoreRenderFinished, throwSwapchainSubOptimal)
import qualified Vulkan as Vk
import qualified Data.Vector as Vector
import Foreign (nullPtr)
import Data.Functor (void)
import Control.Monad (forM)

data Renderer = Renderer { vkContext :: VkContext
                         , pipelines :: TVar [PipelineInfo]
                         , shaders   :: TVar [ShaderInfo]
                         , swapchain :: TVar (Resource Swapchain)
                         }

create
  :: MonadResource m
  => VkContext
  -> Word32
  -- ^ Number of frames in flight
  -> m Renderer
create ctx numFramesInFlight = do
  initialPipelines <- liftIO $ newTVarIO []
  initialShaders <- liftIO $ newTVarIO []

  framebufferSize <- liftIO $ vkGetFramebufferSize ctx

  initialSwapchain <- withSwapchain ctx numFramesInFlight framebufferSize Nothing
  initialSwapchainVar <- liftIO $ newTVarIO initialSwapchain

  pure $ Renderer ctx initialPipelines initialShaders initialSwapchainVar

mkPipeline :: MonadResource m => Renderer -> PipelineInfo -> m (Handle Pipeline)
mkPipeline r pipelineInfo = liftIO . STM.atomically $
  STM.stateTVar (pipelines r) $ \ps ->
    (Handle $ length ps, ps <> [pipelineInfo])

mkShader :: MonadResource m => Renderer -> ShaderInfo -> m (Handle Shader)
mkShader r shaderInfo = liftIO . STM.atomically $
  STM.stateTVar (shaders r) $ \ps ->
    (Handle $ length ps, ps <> [shaderInfo])

withFrame r f = do
  sc <- liftIO $ readTVarIO (swapchain r)

  -- Create attachments
  pipelineInfos <- liftIO $ readTVarIO (pipelines r)
  let attachmentInfos =
        concatMap (renderPassAttachments . pipelineRenderPassLayout) pipelineInfos
  forM attachmentInfos $ \attachmentInfo ->
    case attachmentInfo of
      SwapchainAttachment _ -> pure Nothing


  let
    depthFormat = vkDepthFormat sc
    colorFormat = VkFormat.format $ vkSurfaceFormat sc

  forM pipelineInfos $ \pipelineInfo -> do
    let
      rpLayout = renderPassLayout pipelineInfo
      renderPassCreateInfo =
        Vk.RenderPassCreateInfo
          ()
          Vk.zero
          -- attachmentDescriptions
          (Vector.fromList
           $ toVkAttachmentDescription _swapchainColorFormat _swapchainDepthFormat
           <$> renderPassAttachments rpLayout)
          (Vector.fromList $ renderPassSubpasses rpLayout)
          (Vector.fromList $ renderPassSubpassDependencies rpLayout)

    renderPass <- withRenderPass (vkDevice ctx) renderPassCreateInfo
    pure [renderPass]

  -- Getting the next frame
  withNextFrameInFlight (Resource.get sc) $ \frameSync -> do
    _ <- liftIO $ Vk.waitForFences
      (vkDevice $ vkContext r)
      (Vector.singleton $ fsFenceInFlight frameSync)
      True
      maxBound

    (_, imageIndex) <- liftIO $ throwSwapchainOutOfDate $
      Vk.acquireNextImageKHR
        (vkDevice $ vkContext r)
        (vkSwapchain (Resource.get sc))
        maxBound
        (fsSemaphoreImageAvailable frameSync) -- Current frame different to image index
        Vk.NULL_HANDLE

    draws <- f

    -- sort draws by shader
    -- for each render pass
      -- begin a render pass
      -- set the viewport
      -- set the scissor
      -- bind the pipeline
      -- do our draw

    let
      presentInfo = Vk.PresentInfoKHR
        ()
        (Vector.singleton $ fsSemaphoreRenderFinished frameSync)
        (Vector.singleton $ vkSwapchain $ Resource.get sc)
        (Vector.singleton imageIndex)
        nullPtr

    liftIO $ void $ (throwSwapchainSubOptimal =<<) $ throwSwapchainOutOfDate $
      Vk.queuePresentKHR (vkPresentationQueue $ vkContext r) presentInfo
    pure ()

  pure undefined

withRenderPass
  :: MonadResource m
  => Vk.Device
  -> Vk.RenderPassCreateInfo '[]
  -> m (Resource Vk.RenderPass)
withRenderPass device createInfo = do
  Resource.allocate
    (Vk.createRenderPass device createInfo Nothing)
    (\rp -> Vk.destroyRenderPass device rp Nothing)
