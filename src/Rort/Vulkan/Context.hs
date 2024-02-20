{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Rort.Vulkan.Context where

import qualified Vulkan.Core10.DeviceInitialization as VkDev
import Rort.Window (Window, withVkSurface, getFramebufferSize)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.ByteString.Char8 as BSC
import qualified Vulkan as Vk
import qualified Data.List as List
import Control.Monad (when, forM)
import Control.Monad.IO.Class (liftIO)
import qualified Vulkan.Zero as Vk
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Word (Word32)
import Data.Traversable (for)
import Data.Maybe (catMaybes)
import Data.Bits ((.&.))
import qualified Vulkan.CStruct.Extends as Vk
import Data.Function ((&))
import qualified Data.Set as Set
import Rort.Allocator (Allocator)
import qualified Rort.Allocator as Allocator
import Data.Acquire (Acquire, mkAcquire)

data VkContext = VkContext { vkInstance           :: Vk.Instance
                           , vkSurface            :: Vk.SurfaceKHR
                           , vkPhysicalDevice     :: Vk.PhysicalDevice
                           , vkDevice             :: Vk.Device
                           , vkQueueFamilies      :: QueueFamilies
                           , vkMSAASamples        :: Vk.SampleCountFlagBits
                           , vkGetFramebufferSize :: IO (Int, Int)
                           , vkPresentationQueue  :: Vk.Queue
                           , vkGraphicsQueue      :: Vk.Queue
                           , vkTransferQueue      :: Vk.Queue
                           , vkAllocator          :: Allocator
                           }

data VkSettings
  = VkSettings { requiredExtensions       :: Vector BSC.ByteString
               , requiredValidationLayers :: Vector BSC.ByteString
               , applicationInfo          :: Vk.ApplicationInfo
               }

data QueueFamilies
  = QueueFamilies { graphicsQueueFamilies     :: NonEmpty Word32
                  , presentationQueueFamilies :: NonEmpty Word32
                  , transferQueueFamilies     :: NonEmpty Word32
                  }
  deriving (Eq, Show)

data SwapchainSupportDetails
  = SwapchainSupportDetails { swapchainSupportCapabilities :: Vk.SurfaceCapabilitiesKHR
                            , swapchainSupportFormats :: Vector Vk.SurfaceFormatKHR
                            , swapchainSupportPresentModes :: Vector Vk.PresentModeKHR
                            }
  deriving (Show)

withVkContext
  :: VkSettings
  -> Window
  -> Acquire VkContext
withVkContext cfg win = do
  liftIO $ checkExts (requiredExtensions cfg)
  liftIO $ checkValidationLayers (requiredValidationLayers cfg)

  let
    createInfo =
      Vk.InstanceCreateInfo
        () -- Chain
        Vk.zero -- Flags
        (Just $ applicationInfo cfg)
        (requiredValidationLayers cfg)
        (requiredExtensions cfg)

  inst <- withVkInstance createInfo
  surface <- withVkSurface inst win

  let deviceExts = Vector.fromList ["VK_KHR_swapchain"]

  mDevice <- liftIO $ pickPhysicalDevice inst surface deviceExts

  case mDevice of
    Nothing -> error "No physical device found that supports graphics operations"
    Just (physicalDevice, queFamilies) -> do
      logicalDevice <-
        withVkLogicalDevice
          physicalDevice
          queFamilies
          deviceExts

      allocator <- Allocator.withAllocator
        physicalDevice
        logicalDevice
        inst
        cfg.applicationInfo.apiVersion

      props <- Vk.getPhysicalDeviceProperties physicalDevice
      let samples = getMaxUsableSampleCount props

      gfxQueue <-
        liftIO $ Vk.getDeviceQueue
          logicalDevice
          (NE.head $ graphicsQueueFamilies queFamilies)
          0
      presentQueue <-
        liftIO $ Vk.getDeviceQueue
          logicalDevice
          (NE.head $ presentationQueueFamilies queFamilies)
          0
      transferQueue <-
        liftIO $ Vk.getDeviceQueue
          logicalDevice
          (NE.head $ presentationQueueFamilies queFamilies)
          0

      pure $ VkContext { vkInstance           = inst
                       , vkSurface            = surface
                       , vkPhysicalDevice     = physicalDevice
                       , vkQueueFamilies      = queFamilies
                       , vkDevice             = logicalDevice
                       , vkMSAASamples        = samples
                       , vkGetFramebufferSize = getFramebufferSize win
                       , vkPresentationQueue  = presentQueue
                       , vkGraphicsQueue      = gfxQueue
                       , vkTransferQueue      = transferQueue
                       , vkAllocator          = allocator
                       }
withVkInstance
  :: Vk.InstanceCreateInfo '[]
  -> Acquire Vk.Instance
withVkInstance createInfo =
  mkAcquire
    (Vk.createInstance createInfo Nothing)
    (`Vk.destroyInstance` Nothing)

withVkLogicalDevice
  :: Vk.PhysicalDevice
  -> QueueFamilies
  -> Vector BSC.ByteString
  -> Acquire Vk.Device
withVkLogicalDevice device queFamilies exts = do
  let
    queueCreateInfos = flip foldMap (uniqueQueueFamilies queFamilies) $ \qfIx ->
      [ Vk.DeviceQueueCreateInfo
          ()                     -- Chain
          Vk.zero                -- Flags
          qfIx                   -- Queue family index
          (Vector.singleton 1.0) -- Queue priorities
      ]

    deviceFeatures = Just $ Vk.zero { Vk.samplerAnisotropy = True }

    -- Note: Deprecated, will be ignored by newer implementations, set for
    -- compatibility
    layers = Vector.fromList [ "VK_LAYER_KHRONOS_validation" ]

    logicalDeviceCreateInfo = Vk.DeviceCreateInfo
      ()
      Vk.zero
      (Vector.fromList $ Vk.SomeStruct <$> queueCreateInfos)
      layers
      exts
      deviceFeatures

  mkAcquire
    (Vk.createDevice device logicalDeviceCreateInfo Nothing)
    (`Vk.destroyDevice` Nothing)

uniqueQueueFamilies :: QueueFamilies -> NonEmpty Word32
uniqueQueueFamilies qf =
  (graphicsQueueFamilies qf <> presentationQueueFamilies qf <> transferQueueFamilies qf)
  & NE.toList
  & Set.fromList
  & Set.toList
  & NE.fromList

checkExts :: Vector BSC.ByteString -> IO ()
checkExts requiredExts = do
  (_, availableExtProps) <- Vk.enumerateInstanceExtensionProperties Nothing
  let
    availableExts = Vk.extensionName <$> availableExtProps
    missingExts =
      Vector.toList requiredExts
        List.\\ Vector.toList availableExts

  when (missingExts /= []) $
    error $ "Missing extensions: " <> show missingExts

checkValidationLayers :: Vector BSC.ByteString -> IO ()
checkValidationLayers reqValidationLayers = do
  (_, availableValidationLayers) <- Vk.enumerateInstanceLayerProperties
  let
    availableLayers = Vk.layerName <$> availableValidationLayers
    missingLayers =
      Vector.toList reqValidationLayers
        List.\\ Vector.toList availableLayers

  when (missingLayers /= []) $
    error $ "Missing requested validation layers: " <> show missingLayers

pickPhysicalDevice
  :: Vk.Instance
  -> Vk.SurfaceKHR
  -> Vector BSC.ByteString
  -> IO (Maybe (Vk.PhysicalDevice, QueueFamilies))
pickPhysicalDevice vkInst surface deviceExts = do
  (_, devices) <- Vk.enumeratePhysicalDevices vkInst
  suitableDevices <- fmap Vector.catMaybes $ forM devices $ \dev -> do
    -- Can use these to check if device supports
    props <- Vk.getPhysicalDeviceProperties dev
    feats <- Vk.getPhysicalDeviceFeatures dev
    mqf <- findQueueFamilies surface dev
    case mqf of
      Nothing -> pure Nothing
      Just qf -> do
        suitable <- isDeviceSuitable surface dev props feats deviceExts qf
        if suitable
        then pure $ Just (dev, qf)
        else pure Nothing

  pure $ pickFirst suitableDevices

  where
    pickFirst :: Vector a -> Maybe a
    pickFirst = (Vector.!? 0)

findQueueFamilies :: Vk.SurfaceKHR -> Vk.PhysicalDevice -> IO (Maybe QueueFamilies)
findQueueFamilies surface device = do
  qfs <- Vk.getPhysicalDeviceQueueFamilyProperties device

  mGfxFamilies <-
    for (zip [0..] (Vector.toList qfs)) $ \(ix, fam) -> do
      if supportsGraphicsOperations fam
      then pure $ Just ix
      else pure Nothing

  mTransferFamilies <-
    for (zip [0..] (Vector.toList qfs)) $ \(ix, fam) -> do
      if supportsTransferOperations fam
      then pure $ Just ix
      else pure Nothing

  mPresentationFamilies <-
    for (zip [0..] (Vector.toList qfs)) $ \(ix, _fam) -> do
      supported <- Vk.getPhysicalDeviceSurfaceSupportKHR device ix surface
      if supported
      then pure $ Just ix
      else pure Nothing

  let
    gfxFamilies = catMaybes mGfxFamilies
    presentationFamilies = catMaybes mPresentationFamilies
    transferFamilies = catMaybes mTransferFamilies

  case (NE.nonEmpty gfxFamilies, NE.nonEmpty presentationFamilies, NE.nonEmpty transferFamilies) of
    (Just neGfxFamilies, Just nePresentationFamilies, Just neTransferFamilies) ->
      pure $ Just $ QueueFamilies neGfxFamilies nePresentationFamilies neTransferFamilies
    _ ->
      pure Nothing

  where
    supportsGraphicsOperations :: Vk.QueueFamilyProperties -> Bool
    supportsGraphicsOperations =
      (== Vk.QUEUE_GRAPHICS_BIT) . (.&. Vk.QUEUE_GRAPHICS_BIT) . Vk.queueFlags

    supportsTransferOperations :: Vk.QueueFamilyProperties -> Bool
    supportsTransferOperations =
      (== Vk.QUEUE_TRANSFER_BIT) . (.&. Vk.QUEUE_TRANSFER_BIT) . Vk.queueFlags

isDeviceSuitable
  :: Vk.SurfaceKHR
  -> Vk.PhysicalDevice
  -> Vk.PhysicalDeviceProperties
  -> Vk.PhysicalDeviceFeatures
  -> Vector BSC.ByteString
  -> QueueFamilies
  -> IO Bool
isDeviceSuitable surface device _props feats deviceExts _queueFams = do
  -- We already know the device has suitable queue families thanks to QueueFamilies passed in.
  extsSupported <- checkDeviceExts deviceExts device
  -- Need to check exts first, to confirm that the device actually supports a swap chain
  if not extsSupported
  then pure False
  else do
    swapChainSupport <- querySwapchainSupport device surface
    let
      swapChainAdequate =
        not (null $ swapchainSupportFormats swapChainSupport)
        && not (null $ swapchainSupportPresentModes swapChainSupport)
    pure (swapChainAdequate && Vk.samplerAnisotropy feats)

checkDeviceExts :: Vector BSC.ByteString -> Vk.PhysicalDevice -> IO Bool
checkDeviceExts requiredExts device = do
  (_, exts) <- Vk.enumerateDeviceExtensionProperties device Nothing
  let
    availableExts = Vk.extensionName <$> exts
    missingExts =
      Vector.toList requiredExts
        List.\\ Vector.toList availableExts

  pure $ null missingExts

querySwapchainSupport :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO SwapchainSupportDetails
querySwapchainSupport device surface = do
  capabilities <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR device surface
  (_, formats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR device surface
  (_, presentModes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR device surface
  pure $ SwapchainSupportDetails capabilities formats presentModes

getMaxUsableSampleCount
  :: Vk.PhysicalDeviceProperties -> Vk.SampleCountFlagBits
getMaxUsableSampleCount props =
  let
    counts :: Vk.SampleCountFlags
    counts =
      VkDev.framebufferColorSampleCounts (VkDev.limits props)
      .&.
      VkDev.framebufferDepthSampleCounts (VkDev.limits props)
  in
    if counts .&. Vk.SAMPLE_COUNT_64_BIT == Vk.SAMPLE_COUNT_64_BIT
    then Vk.SAMPLE_COUNT_64_BIT
    else if counts .&. Vk.SAMPLE_COUNT_32_BIT == Vk.SAMPLE_COUNT_32_BIT
    then Vk.SAMPLE_COUNT_32_BIT
    else if counts .&. Vk.SAMPLE_COUNT_16_BIT == Vk.SAMPLE_COUNT_16_BIT
    then Vk.SAMPLE_COUNT_16_BIT
    else if counts .&. Vk.SAMPLE_COUNT_8_BIT == Vk.SAMPLE_COUNT_8_BIT
    then Vk.SAMPLE_COUNT_8_BIT
    else if counts .&. Vk.SAMPLE_COUNT_4_BIT == Vk.SAMPLE_COUNT_4_BIT
    then Vk.SAMPLE_COUNT_4_BIT
    else if counts .&. Vk.SAMPLE_COUNT_2_BIT == Vk.SAMPLE_COUNT_2_BIT
    then Vk.SAMPLE_COUNT_2_BIT
    else Vk.SAMPLE_COUNT_1_BIT
