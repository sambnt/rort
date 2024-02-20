{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Rort.Examples.Uniform where

import Rort.Window (withWindow, getRequiredExtensions, closeWindow, withWindowEvent)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..), graphicsQueueFamilies)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Rort.Render.Swapchain (withSwapchain, vkSwapchain, retryOnSwapchainOutOfDate, Swapchain (vkExtent))
import Rort.Render.FramesInFlight (withNextFrameInFlight, withFramesInFlight, FrameInFlight (FrameInFlight))
import Rort.Vulkan (withVkShaderModule, withVkCommandBuffers, withVkCommandPool, withVkBuffer, withVkBufferMemory, copyBuffer, withVkDescriptorSetLayout)
import qualified Vulkan.Zero as Vk
import qualified Rort.Util.Resource as Resource
import qualified Vulkan.CStruct.Extends as Vk
import Data.Bits ((.|.))
import Rort.Render.Types (SubpassInfo(..), RenderPassInfo(..), Draw(..), DrawCallIndexed(..), BufferRef (..), DrawCall (..), DrawCallPrimitive (..), Subpass (Subpass), RenderPass (renderPassSubpasses, renderPass), frameRenderPasses)
import Rort.Render (mkFrameData, finallyPresent)
import Control.Monad (when, unless, forM_)
import Data.Functor ((<&>))
import Foreign (sizeOf, Word16, castPtr, pokeArray, advancePtr)
import Rort.Window.Types (WindowEvent(..))
import Linear (M44, Quaternion)
import qualified Linear
import qualified Torsor
import Data.Function ((&))
import qualified Rort.Allocator as Allocator
import qualified Chronos
import Control.Lens ((%~))
import UnliftIO.Async (race_, concurrently_)
import Data.Acquire (with)

main :: IO ()
main = do
  let
    width = 800
    height = 600

  withWindow width height "Example: Uniform" $ \win -> do
    windowExts <- getRequiredExtensions win

    runResourceT $ do
      let
        cfg = VkSettings { requiredExtensions =
                             windowExts <> Vector.fromList []
                         , requiredValidationLayers =
                             -- Vector.fromList [ "VK_LAYER_KHRONOS_validation" ]
                             Vector.fromList [ "VK_LAYER_RENDERDOC_Capture" ]
                         , applicationInfo =
                             Vk.ApplicationInfo
                               (Just "Example: Uniform")  -- application name
                               (Vk.MAKE_API_VERSION 1 0 0) -- application version
                               (Just "No engine")          -- engine name
                               (Vk.MAKE_API_VERSION 1 0 0) -- engine version
                               (Vk.MAKE_API_VERSION 1 0 0) -- Vulkan API version (patch version ignored)
                         }

      ctx <- withVkContext cfg win

      vertShaderCode <- liftIO $ BS.readFile "data/uniformBuffer.vert.spv"
      fragShaderCode <- liftIO $ BS.readFile "data/tri.frag.spv"

      let numFramesInFlight = 2
      framesInFlight <-
        withFramesInFlight (vkDevice ctx) (vkQueueFamilies ctx) numFramesInFlight

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

      setLayout <- withVkDescriptorSetLayout (vkDevice ctx)
        $ Vk.DescriptorSetLayoutCreateInfo
            ()
            Vk.zero
            (Vector.fromList [ Vk.DescriptorSetLayoutBinding
                                 0 -- binding in shader
                                 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                 1 -- descriptor count
                                 Vk.SHADER_STAGE_VERTEX_BIT
                                 Vector.empty -- immutable samplers
                             ]
            )

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

      copyCmdPool <-
        withVkCommandPool
          (vkDevice ctx)
          (NE.head . graphicsQueueFamilies $ vkQueueFamilies ctx)

      liftIO $ copyBuffer
        (vkDevice ctx)
        (Resource.get copyCmdPool)
        (vkGraphicsQueue ctx)
        (vertexBufferSize + indexBufferSize)
       (Resource.get stagingBuffer)
        (Resource.get deviceBuffer)

      startTime <- liftIO Chronos.now

      let
        subpassInfo =
          SubpassInfo { subpassInfoShaderStages = pipelineShaderStages
                      , subpassInfoDescriptors = [ Resource.get setLayout ]
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

      retryOnSwapchainOutOfDate ctx initialSwapchain $ \swapchain -> do
        with (mkFrameData ctx swapchain [renderPassInfo]) $ \frameDatas -> do
          -- rendering a frame
          let
            renderLoop = do
              withNextFrameInFlight (vkDevice ctx) framesInFlight $ \(FrameInFlight fs descPool cmdPool) -> runResourceT $ do
                finallyPresent (vkDevice ctx) (vkGraphicsQueue ctx) (vkPresentationQueue ctx) (vkSwapchain swapchain) fs $ \imageIndex -> do
                  set <-
                    -- Not recommended to free descriptor sets, just reset pool. So
                    -- we don't setup a destructor here.
                    fmap Vector.head $ Vk.allocateDescriptorSets
                      (vkDevice ctx)
                      $ Vk.DescriptorSetAllocateInfo
                          ()
                          descPool
                          (Vector.singleton $ Resource.get setLayout)

                  let
                    uniformBufferSize =
                      fromIntegral $ 3 * sizeOf(undefined :: M44 Float)
                  (uniformBuffer, uniformBufferPtr) <- Resource.get
                    <$> Allocator.withUniformBuffer
                          (vkAllocator ctx)
                          uniformBufferSize

                  Vk.updateDescriptorSets (vkDevice ctx)
                    -- writes
                    (fmap Vk.SomeStruct . Vector.singleton
                     $  Vk.WriteDescriptorSet
                          ()
                          set -- dst set
                          0 -- dst binding
                          0 -- dst array element
                          1 -- descriptor count
                          Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER -- type
                          mempty -- image info
                          -- buffer info
                          ( Vector.singleton $
                              Vk.DescriptorBufferInfo
                                uniformBuffer
                                0 -- offset
                                uniformBufferSize
                          )
                          mempty -- texel buffer view
                    )
                    -- copies
                    mempty

                  currentTime <- liftIO Chronos.now
                  let
                    newUniformBufferData =
                      getUniformBufferData
                        startTime
                        currentTime
                        ( fromIntegral . Extent2D.width $ vkExtent swapchain
                        , fromIntegral . Extent2D.height $ vkExtent swapchain
                        )
                  liftIO $ pokeArray (castPtr @() @(M44 Float) $ uniformBufferPtr) newUniformBufferData

                  cmdBuffers <-
                    withVkCommandBuffers
                      (vkDevice ctx)
                      $ Vk.CommandBufferAllocateInfo
                          cmdPool
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
                    frameData = frameDatas !! fromIntegral imageIndex

                  forM_ (frameRenderPasses frameData) $ \(framebuffer, rp) -> do
                    let
                      clearValues = Vector.fromList [Vk.Color $ Vk.Float32 0 0 0 0]
                      renderStartPos = Vk.Offset2D 0 0
                      renderExtent = vkExtent swapchain
                      renderPassBeginInfo =
                        Vk.RenderPassBeginInfo
                          ()
                          (renderPass rp)
                          framebuffer
                          (Vk.Rect2D renderStartPos renderExtent)
                          clearValues

                    Vk.cmdBeginRenderPass
                      cmdBuffer renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE

                    let
                      viewport = Vk.Viewport
                        0 -- startX
                        0 -- startY
                        (fromIntegral $ Extent2D.width renderExtent) -- width
                        (fromIntegral $ Extent2D.height renderExtent) -- height
                        0 -- min depth
                        1 -- max depth
                      scissor = Vk.Rect2D renderStartPos renderExtent
                    Vk.cmdSetViewport
                      cmdBuffer
                      0
                      (Vector.singleton viewport)
                    Vk.cmdSetScissor
                      cmdBuffer
                      0
                      (Vector.singleton scissor)

                    forM_ (renderPassSubpasses rp) $ \(Subpass pipeline pipelineLayout draw) -> do
                      Vk.cmdBindPipeline
                        cmdBuffer
                        Vk.PIPELINE_BIND_POINT_GRAPHICS
                        pipeline

                      let
                        (vertexBufs, vertexOffsets) =
                          unzip
                          $ drawVertexBuffers draw
                          <&> \(BufferRef vertexBuf offset) -> (vertexBuf, offset)
                      unless (null vertexBufs) $
                        Vk.cmdBindVertexBuffers
                          cmdBuffer
                          0 -- first binding
                          (Vector.fromList vertexBufs)
                          (Vector.fromList vertexOffsets)
                      forM_ (drawIndexBuffers draw) $ \(BufferRef indexBuf offset, indexType) -> do
                        Vk.cmdBindIndexBuffer
                          cmdBuffer
                          indexBuf
                          offset
                          indexType

                      Vk.cmdBindDescriptorSets
                        cmdBuffer
                        Vk.PIPELINE_BIND_POINT_GRAPHICS
                        pipelineLayout
                        0 -- first set
                        (Vector.singleton set)
                        mempty -- dynamic offsets

                      case drawCall draw of
                        (IndexedDraw (DrawCallIndexed indexCount instanceCount firstIndex vertexOffset firstInstance)) ->
                          Vk.cmdDrawIndexed
                            cmdBuffer indexCount instanceCount firstIndex vertexOffset firstInstance
                        (PrimitiveDraw (DrawCallPrimitive firstVertex firstInstance instanceCount vertexCount)) ->
                          Vk.cmdDraw
                            cmdBuffer vertexCount instanceCount firstVertex firstInstance

                    Vk.cmdEndRenderPass cmdBuffer

                  Vk.endCommandBuffer cmdBuffer
                  pure cmdBuffer

              renderLoop

            eventLoop :: ResourceT IO ()
            eventLoop = do
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
                  Nothing -> pure True
              when shouldContinue eventLoop

          race_ eventLoop (concurrently_ renderLoop renderLoop)

getUniformBufferData
  :: Chronos.Time -> Chronos.Time -> (Int, Int) -> [M44 Float]
getUniformBufferData startTime currentTime (w, h) = do
  -- This is an abstract time unit, not seconds, not nanoseconds, it's abstract.
  let
    timePassed = currentTime `Torsor.difference` startTime

    asFloat = fromIntegral . Chronos.getTimespan

    rotQ :: Linear.Quaternion Float
    rotQ = Linear.axisAngle (Linear.V3 0 0 1) (asFloat timePassed * ((pi / 2) / asFloat Chronos.second))

    model :: M44 Float
    model = Linear.transpose $ Linear.mkTransformation rotQ (Linear.V3 0 0 0)

    view :: M44 Float
    view = Linear.transpose $ Linear.lookAt
      (Linear.V3 2 2 2) -- Eye position
      (Linear.V3 0 0 0) -- Looking at
      (Linear.V3 0 0 1) -- Up direction

    proj :: M44 Float
    proj = Linear.perspective
      (45.0 * pi / 180.0) -- FOV y in radians
      (fromIntegral w / fromIntegral h)
      0.1 -- Near plane
      10  -- Far plane
      & Linear.transpose
      & Linear._y . Linear._y %~ (* (-1))

    in
      [model, view, proj]
