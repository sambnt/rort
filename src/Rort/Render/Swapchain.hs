{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rort.Render.Swapchain where

import qualified Vulkan as Vk
import qualified Vulkan.Exception as Vk
import Control.Monad.Trans.Resource (MonadResource, runResourceT, MonadUnliftIO)
import qualified Control.Monad.Trans.Resource as ResourceT
import qualified Vulkan.Zero as Vk
import Rort.Vulkan.Context (VkContext, vkDevice, swapchainSupportCapabilities, swapchainSupportPresentModes, querySwapchainSupport, swapchainSupportFormats, vkSurface, vkPhysicalDevice, graphicsQueueFamilies, presentationQueueFamilies, vkQueueFamilies, vkGetFramebufferSize)
import Data.Word (Word32)
import Control.Monad.IO.Class ( liftIO, MonadIO )
import Control.Monad ( forM_, forM )
import Data.Vector (Vector)
import qualified Vulkan.Extensions.VK_KHR_get_surface_capabilities2 as VkE ( SurfaceCapabilitiesKHR(currentTransform, currentExtent, minImageExtent, maxImageExtent, maxImageCount, minImageCount), format, colorSpace )
import qualified Data.Vector as Vector
import qualified Data.List.NonEmpty as NE
import qualified Vulkan.Core10.DeviceInitialization as VkDev
import qualified Vulkan.Extensions.VK_KHR_display_swapchain as VkSwapchain
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import Data.List.NonEmpty (NonEmpty)
import Data.Bits ((.&.))
import Rort.Util.Resource (Resource)
import qualified Rort.Util.Resource as Resource
import Control.Monad.Catch (Exception, handleIf, MonadCatch)
import Control.Exception (throwIO, try, SomeException, fromException)

data Swapchain
  = Swapchain { vkSwapchain                :: Vk.SwapchainKHR
              , vkImageViews               :: Vector Vk.ImageView
              , vkSurfaceFormat            :: Vk.SurfaceFormatKHR
              , vkDepthFormat              :: Vk.Format
              , vkExtent                   :: Vk.Extent2D
              }

data SwapchainOutOfDate = SwapchainOutOfDate
  deriving (Eq, Show)

instance Exception SwapchainOutOfDate

isSwapchainOutOfDate :: SomeException -> Bool
isSwapchainOutOfDate e =
  case fromException e of
    Just SwapchainOutOfDate -> True
    _                       -> False

throwSwapchainOutOfDate :: IO a -> IO a
throwSwapchainOutOfDate action = do
  eResult <- try action
  case eResult of
    (Left (Vk.VulkanException Vk.ERROR_OUT_OF_DATE_KHR)) ->
      throwIO SwapchainOutOfDate
    (Left ex) ->
      throwIO ex
    (Right result) ->
      pure result

throwSwapchainSubOptimal :: Vk.Result -> IO Vk.Result
throwSwapchainSubOptimal Vk.SUBOPTIMAL_KHR = throwIO SwapchainOutOfDate
throwSwapchainSubOptimal result            = pure result

retryOnSwapchainOutOfDate
  :: (MonadCatch m, MonadResource m, MonadUnliftIO m)
  => VkContext
  -> Resource Swapchain
  -> (Swapchain -> ResourceT.ResourceT m a)
  -> m a
retryOnSwapchainOutOfDate ctx initialSwapchain f = do
  handleIf isSwapchainOutOfDate
    (\_ -> do
        framebufferSize <- liftIO $ vkGetFramebufferSize ctx
        swapchain <-
          withSwapchain
            ctx
            framebufferSize
            (Just $ Resource.get initialSwapchain)
        liftIO $ Resource.free initialSwapchain
        -- Retry the computation
        retryOnSwapchainOutOfDate ctx swapchain f
    )
    (runResourceT $ f $ Resource.get initialSwapchain)

withSwapchain
  :: MonadResource m
  => VkContext
  -> (Int, Int) -- ^ Frame buffer size
  -> Maybe Swapchain -- ^ Old swapchain
  -> m (Resource Swapchain)
withSwapchain ctx framebufferSize mOldSwapchain = do
  sci <- liftIO $ mkSwapchainCreateInfo
           (vkPhysicalDevice ctx)
           (vkSurface ctx)
           framebufferSize
           (NE.head $ graphicsQueueFamilies $ vkQueueFamilies ctx)
           (NE.head $ presentationQueueFamilies $ vkQueueFamilies ctx)
           (maybe Vk.NULL_HANDLE vkSwapchain mOldSwapchain)
  depthFormat <- liftIO $ findDepthFormat (vkPhysicalDevice ctx)

  let format = VkSwapchain.imageFormat sci
      colorSpace = VkSwapchain.imageColorSpace sci

  sc <- Resource.allocate
    (Vk.createSwapchainKHR (vkDevice ctx) sci Nothing)
    (\sc -> Vk.destroySwapchainKHR (vkDevice ctx) sc Nothing)
  (_, images) <- liftIO $ Vk.getSwapchainImagesKHR (vkDevice ctx) (Resource.get sc)
  ivs <- Resource.allocate
    (forM images (createSwapchainImageView (vkDevice ctx) format))
    (\ivs -> forM_ ivs (\iv -> Vk.destroyImageView (vkDevice ctx) iv Nothing))

  pure $ Swapchain <$> sc
                   <*> ivs
                   <*> pure (Vk.SurfaceFormatKHR format colorSpace)
                   <*> pure depthFormat
                   <*> pure (VkSwapchain.imageExtent sci)

mkSwapchainCreateInfo
  :: MonadIO m
  => VkDev.PhysicalDevice
  -> VkSwapchain.SurfaceKHR
  -> (Int, Int)
  -> Word32
  -> Word32
  -> VkSwapchain.SwapchainKHR
  -> m (VkSwapchain.SwapchainCreateInfoKHR '[])
mkSwapchainCreateInfo physicalDevice surface frameBufferSize graphicsQueueIx presentQueueIx oldSwapchain = do
  -- NOTE: Swap chain support must be re-queried everytime the swapchain is
  -- re-created, hence why this call is here.
  swapchainSupport <- liftIO $ querySwapchainSupport physicalDevice surface

  surfaceFormat <-
    if Vector.null $ swapchainSupportFormats swapchainSupport
    then error "Swap chain does not support any formats"
    else pure $ chooseSwapSurfaceFormat (NE.fromList $ Vector.toList $ swapchainSupportFormats swapchainSupport)

  presentMode <-
    if Vector.null $ swapchainSupportPresentModes swapchainSupport
    then error "Swap chain does not support any present modes"
    else pure $ chooseSwapPresentMode (NE.fromList $ Vector.toList $ swapchainSupportPresentModes swapchainSupport)

  let extent = chooseSwapExtent (swapchainSupportCapabilities swapchainSupport) frameBufferSize

  -- Choose number of images in the swap chain, We aim to use as many as possible.
  -- The implementation has a minimum, but it's recommended to use at least one more image than the minimum.
  let
    maxImages = VkE.maxImageCount $ swapchainSupportCapabilities swapchainSupport
    minImages = VkE.minImageCount $ swapchainSupportCapabilities swapchainSupport
    imageCount =
      -- (maxImages == 0) means no limit
      if maxImages > 0 && minImages + 1 > maxImages
      then maxImages
      else minImages + 1

    (imageSharingMode, queueFamilyIndices) =
      if graphicsQueueIx /= presentQueueIx
      then (Vk.SHARING_MODE_CONCURRENT, Vector.fromList [graphicsQueueIx, presentQueueIx])
      else (Vk.SHARING_MODE_EXCLUSIVE, mempty)

  pure $
    Vk.SwapchainCreateInfoKHR
      () -- Chain
      Vk.zero -- Flags
      surface
      imageCount
      (VkE.format surfaceFormat)
      (VkE.colorSpace surfaceFormat)
      extent
      1 -- imageArrayLayers, always one for non-steroscopic 3D applications
      Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT -- intended usage of swapchain images
      imageSharingMode
      queueFamilyIndices
      (VkE.currentTransform $ swapchainSupportCapabilities swapchainSupport)
      Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR -- Blending with other windows in the window system
      presentMode
      True -- Clip pixels obscured by another window
      oldSwapchain

createSwapchainImageView
  :: MonadIO m
  => Vk.Device
  -> Vk.Format
  -> Vk.Image
  -> m Vk.ImageView
createSwapchainImageView logicalDevice surfaceFormat img =
  let
    imgInfo :: Vk.ImageViewCreateInfo '[]
    imgInfo =
      Vk.ImageViewCreateInfo
        Vk.zero
        Vk.zero
        img
        Vk.IMAGE_VIEW_TYPE_2D
        surfaceFormat
        (Vk.ComponentMapping Vk.COMPONENT_SWIZZLE_IDENTITY
                             Vk.COMPONENT_SWIZZLE_IDENTITY
                             Vk.COMPONENT_SWIZZLE_IDENTITY
                             Vk.COMPONENT_SWIZZLE_IDENTITY
        )
        (Vk.ImageSubresourceRange
          Vk.IMAGE_ASPECT_COLOR_BIT -- Colour target
          0 -- Base mipmap level
          1 -- Mipmap levels
          0 -- Base layer
          1 -- Layer count
        )
  in
    liftIO $ Vk.createImageView logicalDevice imgInfo Nothing

findDepthFormat :: VkDev.PhysicalDevice -> IO VkDev.Format
findDepthFormat device =
  findSupportedFormat device
    [Vk.FORMAT_D32_SFLOAT, Vk.FORMAT_D32_SFLOAT_S8_UINT, Vk.FORMAT_D24_UNORM_S8_UINT]
    Vk.IMAGE_TILING_OPTIMAL
    Vk.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT

findSupportedFormat
  :: Vk.PhysicalDevice
  -> [Vk.Format]
  -> Vk.ImageTiling
  -> Vk.FormatFeatureFlags
  -> IO Vk.Format
findSupportedFormat device candidates tiling features = do
  formats <- flip foldMap candidates $ \candidateFormat -> do
    props <- Vk.getPhysicalDeviceFormatProperties device candidateFormat
    if (tiling == Vk.IMAGE_TILING_LINEAR && (VkDev.linearTilingFeatures props .&. features) == features)
       || (tiling == Vk.IMAGE_TILING_OPTIMAL && (VkDev.optimalTilingFeatures props .&. features) == features)
    then pure [candidateFormat]
    else pure []

  case formats of
    []  -> error "Failed to find supported format!"
    f:_ -> pure f

chooseSwapSurfaceFormat :: NonEmpty Vk.SurfaceFormatKHR -> Vk.SurfaceFormatKHR
chooseSwapSurfaceFormat formats =
  -- The format specifies the colour channels of the swap chain surface. E.g.
  -- VK_FORMAT_B8G8R8A8_SRGB stores four channels, in BGRA order, in the SRGB
  -- colour space.

  -- We do our best to choose a preferred format:
  let
    preferredFormats = flip foldMap (NE.toList formats) $ \f ->
      [f | VkE.format f == Vk.FORMAT_B8G8R8A8_SRGB
           && VkE.colorSpace f == Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR
      ]
  in
    -- If we can't find our preferred format, just use the first format
    case preferredFormats of
      []    -> NE.head formats
      (x:_) -> x

chooseSwapPresentMode :: NonEmpty Vk.PresentModeKHR -> Vk.PresentModeKHR
chooseSwapPresentMode presentModes =
  -- https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain
  -- VK_PRESENT_MODE_IMMEDIATE_KHR:
  --   Images submitted go straight to the screen, tearing.
  -- VK_PRESENT_MODE_FIFO_KHR:
  --   The swap chain is a queue where the display takes an image from the
  --   front of the queue when the display is refreshed and the program
  --   inserts rendered images at the back of the queue. If the queue is full
  --   then the program has to wait. This is most similar to vertical sync as
  --   found in modern games. The moment that the display is refreshed is
  --   known as "vertical blank".
  -- VK_PRESENT_MODE_FIFO_RELAXED_KHR:
  --   This mode only differs from the previous one if the application is late
  --   and the queue was empty at the last vertical blank. Instead of waiting
  --   for the next vertical blank, the image is transferred right away when
  --   it finally arrives. This may result in visible tearing.
  -- VK_PRESENT_MODE_MAILBOX_KHR:
  --   This is another variation of the second mode. Instead of blocking the
  --   application when the queue is full, the images that are already queued
  --   are simply replaced with the newer ones. This mode can be used to
  --   render frames as fast as possible while still avoiding tearing,
  --   resulting in fewer latency issues than standard vertical sync. This is
  --   commonly known as "triple buffering", although the existence of three
  --   buffers alone does not necessarily mean that the framerate is unlocked.

  let
    preferredModes = flip foldMap (NE.toList presentModes) $ \p ->
      -- We prefer mailbox (triple buffering)
      [p | p == Vk.PRESENT_MODE_MAILBOX_KHR]
  in
    -- If we can't find our preferred format, just use the first format
    case preferredModes of
      -- Only VK_PRESENT_MODE_FIFO_KHR is guaranteed to be available.
      []    -> Vk.PRESENT_MODE_FIFO_KHR
      (x:_) -> x

-- Resolution of swap chain images.
chooseSwapExtent
  :: Vk.SurfaceCapabilitiesKHR
  -> (Int, Int)
  -- ^ Framebuffer size in pixels
  -> Vk.Extent2D
chooseSwapExtent capabilities (w, h) =
  if Extent2D.width (VkE.currentExtent capabilities) /= maxBound @Word32
  -- If the current extent isn't set to a special value (maxBound), we can just
  -- use the current extent.
  then VkE.currentExtent capabilities
  -- If it is set to the special value, we have to use the size of the framebuffer.
  else
    let
      minWidth, maxWidth :: Int
      minWidth = fromIntegral $ Extent2D.width $ VkE.minImageExtent capabilities
      maxWidth = fromIntegral $ Extent2D.width $ VkE.maxImageExtent capabilities

      minHeight, maxHeight :: Int
      minHeight = fromIntegral $ Extent2D.height $ VkE.minImageExtent capabilities
      maxHeight = fromIntegral $ Extent2D.height $ VkE.maxImageExtent capabilities

      clamp (mn, mx) a
        | a < mn    = mn
        | a > mx    = mx
        | otherwise = a
    in
      Vk.Extent2D
        (fromIntegral $ clamp (minWidth, maxWidth) w)
        (fromIntegral $ clamp (minHeight, maxHeight) h)
