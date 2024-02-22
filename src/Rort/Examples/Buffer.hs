{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rort.Examples.Buffer where

import Rort.Window (withWindow, getRequiredExtensions, withWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..), graphicsQueueFamilies)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Rort.Render.Swapchain (withSwapchain, vkSwapchain, retryOnSwapchainOutOfDate)
import Rort.Render.FramesInFlight (withNextFrameInFlight, withFramesInFlight, FrameInFlight (..))
import Rort.Vulkan (withVkShaderModule, withVkCommandBuffers, withVkCommandPool, withVkBuffer, withVkBufferMemory, copyBuffer)
import qualified Vulkan.Zero as Vk
import Data.Bits ((.|.))
import Rort.Render.Types (SubpassInfo(..), RenderPassInfo(..), Draw(..), DrawCallIndexed(..), BufferRef (..), DrawCall (IndexedDraw))
import Rort.Render (mkFrameData, recordFrameData, finallyPresent)
import Control.Monad (when)
import Foreign (sizeOf, Word16, castPtr, pokeArray, advancePtr)
import Rort.Window.Types (WindowEvent(..))
import Data.Acquire (with, allocateAcquire, Acquire)

initExample :: VkContext -> Acquire [RenderPassInfo]
initExample ctx = do
  vertShaderCode <- liftIO $ BS.readFile "data/vertexBuffers.vert.spv"
  fragShaderCode <- liftIO $ BS.readFile "data/tri.frag.spv"

  vertShader <-
    withVkShaderModule (vkDevice ctx)
      $ Vk.ShaderModuleCreateInfo () Vk.zero vertShaderCode
  fragShader <-
    withVkShaderModule (vkDevice ctx)
      $ Vk.ShaderModuleCreateInfo () Vk.zero fragShaderCode

  let
    pipelineShaderStages =
      [ Vk.PipelineShaderStageCreateInfo
          ()
          Vk.zero
          Vk.SHADER_STAGE_VERTEX_BIT
          vertShader
          "main"
          Nothing
      , Vk.PipelineShaderStageCreateInfo
          ()
          Vk.zero
          Vk.SHADER_STAGE_FRAGMENT_BIT
          fragShader
          "main"
          Nothing
      ]

  copyCmdPool <-
    withVkCommandPool
      (vkDevice ctx)
      (NE.head . graphicsQueueFamilies $ vkQueueFamilies ctx)

  let
    vertices :: [Float]
    vertices = [ -0.5, -0.5, 1, 0, 0
               ,  0.5, -0.5, 0, 1, 0
               ,  0.5,  0.5, 0, 0, 1
               , -0.5,  0.5, 1, 1, 1
               ]
    vertexBufferSize = fromIntegral $
      sizeOf (undefined :: Float) * length vertices

    indices :: [Word16]
    indices = [ 2, 1, 0, 0, 3, 2 ]

    indexBufferSize = fromIntegral $
      sizeOf (undefined :: Word16) * length indices

  stagingBuffer <- withVkBuffer (vkDevice ctx)
    $ Vk.BufferCreateInfo
        ()
        Vk.zero
        (vertexBufferSize + indexBufferSize)
        (Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT .|. Vk.BUFFER_USAGE_INDEX_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_SRC_BIT)
        Vk.SHARING_MODE_EXCLUSIVE
        Vector.empty -- queue family indices, ignored.
  stagingBufferMem <-
    withVkBufferMemory
      (vkPhysicalDevice ctx)
      (vkDevice ctx)
      stagingBuffer
      (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
  liftIO $ Vk.bindBufferMemory
    (vkDevice ctx)
    stagingBuffer
    stagingBufferMem
    0 -- offset
  ptr <- liftIO $ Vk.mapMemory
    (vkDevice ctx)
    stagingBufferMem
    0 -- offset
    (vertexBufferSize + indexBufferSize)
    Vk.zero
  let vertexPtr = castPtr @() @Float ptr
  liftIO $ pokeArray vertexPtr vertices
  let indexPtr = castPtr @Float @Word16 (advancePtr vertexPtr $ length vertices)
  liftIO $ pokeArray indexPtr indices

  deviceBuffer <- withVkBuffer (vkDevice ctx)
    $ Vk.BufferCreateInfo
        ()
        Vk.zero
        (vertexBufferSize + indexBufferSize)
        (Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT .|. Vk.BUFFER_USAGE_INDEX_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT)
        Vk.SHARING_MODE_EXCLUSIVE
        Vector.empty -- queue family indices, ignored.
  deviceBufferMem <-
    withVkBufferMemory
      (vkPhysicalDevice ctx)
      (vkDevice ctx)
      deviceBuffer
      Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  liftIO $ Vk.bindBufferMemory
    (vkDevice ctx)
    deviceBuffer
    deviceBufferMem
    0 -- offset

  liftIO $ copyBuffer
    (vkDevice ctx)
    copyCmdPool
    (vkGraphicsQueue ctx)
    (vertexBufferSize + indexBufferSize)
    stagingBuffer
    deviceBuffer

  let
    subpassInfo =
      SubpassInfo { subpassInfoShaderStages = pipelineShaderStages
                  , subpassInfoDescriptors = []
                  , subpassInfoVertexBindings =
                      [ Vk.VertexInputBindingDescription
                          0 -- first vertex buffer bound
                          (fromIntegral $ sizeOf (undefined :: Float) * 5)
                          Vk.VERTEX_INPUT_RATE_VERTEX
                      ]
                  , subpassInfoVertexAttributes =
                    [ Vk.VertexInputAttributeDescription
                        0 -- location (pos)
                        0 -- binding
                        Vk.FORMAT_R32G32_SFLOAT
                        0 -- offset
                    , Vk.VertexInputAttributeDescription
                        1 -- location (color)
                        0 -- binding
                        Vk.FORMAT_R32G32B32_SFLOAT
                        (fromIntegral $ sizeOf (undefined :: Float) * 2)
                    ]
                  , subpassInfoDraw =
                      Draw { drawCall = IndexedDraw $ DrawCallIndexed
                               { drawCallIndexedIndexCount = 6
                               , drawCallIndexedInstanceCount = 1
                               , drawCallIndexedFirstIndex = 0
                               , drawCallIndexedVertexOffset = 0
                               , drawCallIndexedFirstInstance = 0
                               }
                           , drawVertexBuffers =
                              [ BufferRef deviceBuffer 0 ]
                           , drawIndexBuffers =
                              [ ( BufferRef deviceBuffer vertexBufferSize
                                , Vk.INDEX_TYPE_UINT16
                                )
                              ]
                           }
                  }
    renderPassInfo = RenderPassInfo [subpassInfo]

  pure [renderPassInfo]

main :: IO ()
main = do
  let
    width = 800
    height = 600

  withWindow width height "Example: Buffer" $ \win -> do
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
      framesInFlight <-
        withFramesInFlight (vkDevice ctx) (vkQueueFamilies ctx) numFramesInFlight

      framebufferSize <- liftIO $ vkGetFramebufferSize ctx
      initialSwapchain <-
        allocateAcquire $ withSwapchain ctx framebufferSize Nothing

      with (initExample ctx) $ \renderPassInfos -> do
        retryOnSwapchainOutOfDate ctx initialSwapchain $ \swapchain -> do
          with (mkFrameData ctx swapchain renderPassInfos) $ \frameDatas -> do
            -- rendering a frame
            let
              -- loop :: ResourceT IO ()
              loop = do
                withNextFrameInFlight (vkDevice ctx) framesInFlight $ \(FrameInFlight fs _descPool cmdPool) -> runResourceT $ do
                  finallyPresent (vkDevice ctx) (vkGraphicsQueue ctx) (vkPresentationQueue ctx) (vkSwapchain swapchain) fs $ \imageIndex -> do
                    (_, cmdBuffers) <-
                      allocateAcquire $ withVkCommandBuffers
                        (vkDevice ctx)
                        $ Vk.CommandBufferAllocateInfo
                            cmdPool
                            -- Primary = can be submitted to queue for execution, can't be
                            -- called by other command buffers.
                            Vk.COMMAND_BUFFER_LEVEL_PRIMARY
                            1 -- count

                    let cmdBuffer = Vector.head cmdBuffers
                    Vk.beginCommandBuffer cmdBuffer
                      $ Vk.CommandBufferBeginInfo
                          ()
                          Vk.zero
                          Nothing -- Inheritance info

                    let
                      frameData = frameDatas !! fromIntegral imageIndex

                    recordFrameData cmdBuffer swapchain frameData

                    Vk.endCommandBuffer cmdBuffer
                    pure cmdBuffer

                shouldContinue <- liftIO $ withWindowEvent win $ \mEv -> do
                  case mEv of
                    Just (WindowError err) -> do
                      putStrLn $ "Error " <> show err
                      closeWindow win
                      pure False
                    Just WindowClose -> do
                      putStrLn "Window closing..."
                      closeWindow win
                      pure False
                    Just (WindowResize x y) -> do
                      putStrLn $ "Window resizing (" <> show x <> ", " <> show y <> ")"
                      pure True
                    Nothing ->
                      pure True
                when shouldContinue loop
            loop
