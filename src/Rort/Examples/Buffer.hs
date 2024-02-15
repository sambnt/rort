{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Rort.Examples.Buffer where

import Rort.Window (withWindow, getRequiredExtensions, getWindowEvent, closeWindow)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..), graphicsQueueFamilies)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Rort.Render.Swapchain (withSwapchain, vkSwapchain, throwSwapchainOutOfDate, throwSwapchainSubOptimal, retryOnSwapchainOutOfDate)
import Rort.Render.FramesInFlight (fsSemaphoreRenderFinished, fsSemaphoreImageAvailable, fsFenceInFlight, withNextFrameInFlight, withFramesInFlight, FrameInFlight (..))
import Rort.Vulkan (withVkShaderModule, withVkCommandBuffers, withVkCommandPool, withVkBuffer, withVkBufferMemory, copyBuffer)
import qualified Vulkan.Zero as Vk
import qualified Rort.Util.Resource as Resource
import qualified Vulkan.CStruct.Extends as Vk
import Data.Bits ((.|.))
import Rort.Render.Types (SubpassInfo(..), RenderPassInfo(..), Draw(..), DrawCallIndexed(..), BufferRef (..), DrawCall (IndexedDraw))
import Rort.Render (mkFrameData, recordFrameData)
import Control.Monad (when)
import Data.Functor (void)
import Foreign (nullPtr, sizeOf, Word16, castPtr, pokeArray, advancePtr)
import Rort.Window.Types (WindowEvent(..))

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

      ctx <- withVkContext cfg win

      vertShaderCode <- liftIO $ BS.readFile "data/vertexBuffers.vert.spv"
      fragShaderCode <- liftIO $ BS.readFile "data/tri.frag.spv"

      let numFramesInFlight = 2
      framesInFlight <-
        withFramesInFlight (vkDevice ctx) numFramesInFlight

      framebufferSize <- liftIO $ vkGetFramebufferSize ctx
      initialSwapchain <-
        withSwapchain ctx framebufferSize Nothing

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
              (Resource.get vertShader)
              "main"
              Nothing
          , Vk.PipelineShaderStageCreateInfo
              ()
              Vk.zero
              Vk.SHADER_STAGE_FRAGMENT_BIT
              (Resource.get fragShader)
              "main"
              Nothing
          ]

      cmdPool <-
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
          (Resource.get stagingBuffer)
          (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
      liftIO $ Vk.bindBufferMemory
        (vkDevice ctx)
        (Resource.get stagingBuffer)
        (Resource.get stagingBufferMem)
        0 -- offset
      ptr <- liftIO $ Vk.mapMemory
        (vkDevice ctx)
        (Resource.get stagingBufferMem)
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
          (Resource.get deviceBuffer)
          Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
      liftIO $ Vk.bindBufferMemory
        (vkDevice ctx)
        (Resource.get deviceBuffer)
        (Resource.get deviceBufferMem)
        0 -- offset

      liftIO $ copyBuffer
        (vkDevice ctx)
        (Resource.get cmdPool)
        (vkGraphicsQueue ctx)
        (vertexBufferSize + indexBufferSize)
        (Resource.get stagingBuffer)
        (Resource.get deviceBuffer)

      retryOnSwapchainOutOfDate ctx initialSwapchain $ \swapchain -> do
        -- BEGIN swapchain-dependent
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
                                    [ BufferRef (Resource.get deviceBuffer) 0 ]
                                 , drawIndexBuffers =
                                    [ ( BufferRef (Resource.get deviceBuffer) vertexBufferSize
                                      , Vk.INDEX_TYPE_UINT16
                                      )
                                    ]
                                 }
                        }
          renderPassInfo = RenderPassInfo [subpassInfo]
        frameDatas <-
          mkFrameData
            ctx
            swapchain
            [renderPassInfo]
        -- END swapchain-dependent

        -- rendering a frame
        let
          -- loop :: ResourceT IO ()
          loop = do
            withNextFrameInFlight (vkDevice ctx) framesInFlight $ \(FrameInFlight fs _descPool) -> runResourceT $ do
              void $ Vk.waitForFences
                (vkDevice ctx)
                (Vector.singleton $ fsFenceInFlight fs)
                True
                maxBound

              (_, imageIndex) <- liftIO $ throwSwapchainOutOfDate $
                Vk.acquireNextImageKHR
                  (vkDevice ctx)
                  (vkSwapchain swapchain)
                  maxBound
                  (fsSemaphoreImageAvailable fs)
                  Vk.NULL_HANDLE

              -- Only reset fences if we know we are submitting work, otherwise we
              -- might deadlock later while waiting for the fence to signal.
              liftIO $ Vk.resetFences
                (vkDevice ctx)
                (Vector.singleton $ fsFenceInFlight fs)

              cmdBuffers <-
                withVkCommandBuffers
                  (vkDevice ctx)
                  $ Vk.CommandBufferAllocateInfo
                      (Resource.get cmdPool)
                      -- Primary = can be submitted to queue for execution, can't be
                      -- called by other command buffers.
                      Vk.COMMAND_BUFFER_LEVEL_PRIMARY
                      1 -- count

              let cmdBuffer = Vector.head $ Resource.get cmdBuffers
              Vk.beginCommandBuffer cmdBuffer
                $ Vk.CommandBufferBeginInfo
                    ()
                    Vk.zero
                    Nothing -- Inheritance info

              let
                frameData = Resource.get frameDatas !! fromIntegral imageIndex

              recordFrameData cmdBuffer swapchain frameData

              Vk.endCommandBuffer cmdBuffer

              let
                submitInfo =
                  Vk.SubmitInfo
                    ()
                    (Vector.singleton $ fsSemaphoreImageAvailable fs)
                    (Vector.singleton Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                    (Vector.singleton $ Vk.commandBufferHandle cmdBuffer)
                    (Vector.singleton $ fsSemaphoreRenderFinished fs)
              liftIO $ Vk.queueSubmit
                (vkGraphicsQueue ctx)
                (Vector.singleton $ Vk.SomeStruct submitInfo)
                (fsFenceInFlight fs)

              let
                presentInfo = Vk.PresentInfoKHR
                  ()
                  (Vector.singleton $ fsSemaphoreRenderFinished fs)
                  (Vector.singleton $ vkSwapchain swapchain)
                  (Vector.singleton imageIndex)
                  nullPtr
              liftIO $ void
                $ (throwSwapchainSubOptimal =<<)
                $ throwSwapchainOutOfDate
                $ Vk.queuePresentKHR (vkPresentationQueue ctx) presentInfo

            mEv <- liftIO $ getWindowEvent win
            shouldContinue <- liftIO $ case mEv of
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
              Nothing -> pure True
            when shouldContinue loop
        loop

