{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rort.Examples.Texture where

import Rort.Window (withWindow, getRequiredExtensions, closeWindow, withWindowEvent)
import Rort.Vulkan.Context (withVkContext, VkSettings (..), VkContext (..), graphicsQueueFamilies)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import qualified Data.Vector as Vector
import qualified Vulkan as Vk
import Control.Monad.IO.Class (liftIO)
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Rort.Render.Swapchain (Swapchain (vkExtent))
import Rort.Vulkan (withVkShaderModule, withVkCommandBuffers, withVkCommandPool, withVkBuffer, withVkBufferMemory, copyBuffer, withVkDescriptorSetLayout)
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk
import Data.Bits ((.|.))
import Rort.Render.Types (SubpassInfo(..), RenderPassInfo(..), Draw(..), DrawCallIndexed(..), BufferRef (..), DrawCall (..), DrawCallPrimitive (..), Subpass (Subpass), RenderPass (renderPassSubpasses, renderPass), frameRenderPasses)
import Rort.Render (mkFrameData, withRenderer, step)
import Control.Monad (when, unless, forM_)
import Data.Functor ((<&>))
import Foreign (sizeOf, Word16, castPtr, pokeArray, advancePtr, peekArray)
import Rort.Window.Types (WindowEvent(..))
import Linear (M44, Quaternion)
import qualified Linear
import qualified Torsor
import Data.Function ((&))
import qualified Rort.Allocator as Allocator
import qualified Chronos
import Control.Lens ((%~))
import UnliftIO.Async (race_, concurrently_)
import Data.Acquire (with, Acquire, allocateAcquire)
import Data.Word (Word8)
import Data.Bitmap (withBitmap)
import qualified Codec.Image.STB as STB
import Rort.Allocator (withImage)

initExample
  :: VkContext
  -> Acquire (Chronos.Time, Vk.DescriptorSetLayout, RenderPassInfo)
initExample ctx = do
  vertShaderCode <- liftIO $ BS.readFile "data/uniformBuffer.vert.spv"
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

  copyCmdPool <-
    withVkCommandPool
      (vkDevice ctx)
      (NE.head . graphicsQueueFamilies $ vkQueueFamilies ctx)

  liftIO $ copyBuffer
    (vkDevice ctx)
    copyCmdPool
    (vkGraphicsQueue ctx)
    (vertexBufferSize + indexBufferSize)
    stagingBuffer
    deviceBuffer

  startTime <- liftIO Chronos.now

  texture <- liftIO $ loadTexture "data/texture.jpg"
  (imgData :: [Word8], (w, h)) <-
    liftIO $ withBitmap texture $ \(w,h) chan _padd ptr -> do
      dat <- peekArray (w * h * 4) ptr
      pure (dat, (w, h))
  let imgDataSize = fromIntegral $ w * h * 4

  let
    imageCreateInfo = Vk.ImageCreateInfo
      ()
      Vk.zero
      Vk.IMAGE_TYPE_2D -- imageType
      Vk.FORMAT_R8G8B8A8_SRGB -- format
      (Vk.Extent3D (fromIntegral w) (fromIntegral h) 1) -- extent
      1 -- mipLevels
      1 -- arrayLayers
      Vk.SAMPLE_COUNT_1_BIT -- samples
      Vk.IMAGE_TILING_OPTIMAL -- tiling
      (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) -- usage
      Vk.SHARING_MODE_EXCLUSIVE -- sharingMode
      Vector.empty -- queueFamilyIndices, ignored because SHARING_MODE not "concurrent"
      Vk.IMAGE_LAYOUT_UNDEFINED -- initialLayout
  img <- withImage (vkAllocator ctx) imageCreateInfo imgDataSize

  let
    subpassInfo =
      SubpassInfo { subpassInfoShaderStages = pipelineShaderStages
                  , subpassInfoDescriptors = [ setLayout ]
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

  pure (startTime, setLayout, renderPassInfo)

main :: IO ()
main = do
  let
    width = 800
    height = 600

  withWindow width height "Example: Uniform" $ \win -> do
    windowExts <- getRequiredExtensions win

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

    with (withVkContext cfg win) $ \ctx -> runResourceT $ do
      with (initExample ctx) $ \(startTime, setLayout, renderPassInfo) -> do
        let numFramesInFlight = 2

        renderer <-
          withRenderer
            ctx
            numFramesInFlight
            (\swapchain -> mkFrameData ctx swapchain [renderPassInfo])
            -- rendering a frame
        let
          renderLoop = do
            step ctx renderer $ \frameData swapchain descPool cmdPool -> do
              set <-
                -- Not recommended to free descriptor sets, just reset pool. So
                -- we don't setup a destructor here.
                fmap Vector.head $ Vk.allocateDescriptorSets
                  (vkDevice ctx)
                  $ Vk.DescriptorSetAllocateInfo
                      ()
                      descPool
                      (Vector.singleton setLayout)

              let
                uniformBufferSize =
                  fromIntegral $ 3 * sizeOf(undefined :: M44 Float)
              (_, (uniformBuffer, uniformBufferPtr)) <-
                allocateAcquire $ Allocator.withUniformBuffer
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
                  closeWindow win
                  pure False
                Just WindowClose -> do
                  closeWindow win
                  pure False
                Just (WindowResize x y) -> do
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

loadTexture :: FilePath -> IO STB.Image
loadTexture path = do
  let numComponents = 4 -- Load with four components (alpha channel)
  eImg <- STB.loadImage' path numComponents
  case eImg of
    Left err -> error $ "Failed to load image, error was: '" <> show err <> "'"
    Right img -> pure img
