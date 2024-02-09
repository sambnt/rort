{-# LANGUAGE OverloadedRecordDot #-}

module Rort.Render where

import Control.Monad.Trans.Resource (MonadResource)
import Rort.Vulkan.Context (VkContext)
import Rort.Render.Swapchain (Swapchain, vkSurfaceFormat, vkImageViews, vkExtent)
import Rort.Render.Types (RenderPassInfo, FrameData, DrawCall (PrimitiveDraw, IndexedDraw), Subpass (Subpass, subpassDraw, subpassPipeline, subpassDescriptorSetLayout), DrawCallPrimitive(..), RenderPass(..), frameRenderPasses, FrameData(FrameData), subpassPipelineLayout)
import Rort.Util.Resource (Resource)
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map
import qualified Vulkan.Core10.FundamentalTypes as Extent2D (Extent2D(width, height))
import Rort.Vulkan.Context (VkContext (..))
import Rort.Vulkan (withVkRenderPass, withVkGraphicsPipelines, withVkFramebuffer, withVkDescriptorPool, withVkPipelineLayout, withVkDescriptorSetLayout)
import qualified Vulkan.Extensions.VK_KHR_surface as VkFormat
import qualified Rort.Util.Resource as Resource
import qualified Vulkan.CStruct.Extends as Vk
import Data.Bits ((.|.))
import Control.Monad (forM, forM_, unless)
import Data.Functor ((<&>))
import Rort.Render.Types (SubpassInfo(..), Draw (..), DrawCallIndexed (..), BufferRef (..), RenderPassInfo (..))
import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import Data.Word (Word32)
import Data.Vector (Vector)

mkFrameData
  :: MonadResource m
  => VkContext
  -> Swapchain
  -> [RenderPassInfo]
  -> m (Resource [FrameData])
mkFrameData ctx swapchain renderPassInfos = do
  renderPasses <- forM renderPassInfos $ \renderPassInfo -> do
    rp <- withVkRenderPass (vkDevice ctx)
           $ Vk.RenderPassCreateInfo
               ()
               Vk.zero
               -- attachment descriptions
               ( Vector.singleton $ Vk.AttachmentDescription
                   Vk.zero
                   (VkFormat.format $ vkSurfaceFormat swapchain)
                   Vk.SAMPLE_COUNT_1_BIT            -- Samples
                   Vk.ATTACHMENT_LOAD_OP_CLEAR      -- Load op, what to do with framebuffer before rendering
                   Vk.ATTACHMENT_STORE_OP_STORE     -- Store op, store the framebuffer
                   Vk.ATTACHMENT_LOAD_OP_DONT_CARE  -- Stencil load op
                   Vk.ATTACHMENT_STORE_OP_DONT_CARE -- Stencil store op
                   Vk.IMAGE_LAYOUT_UNDEFINED        -- Initial layout
                   Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR  -- Final layout
               )
               -- subpass descriptions
               ( Vector.singleton $ Vk.SubpassDescription
                   Vk.zero
                   Vk.PIPELINE_BIND_POINT_GRAPHICS
                   Vector.empty -- input attachments
                   -- color attachments
                   ( Vector.singleton $ Vk.AttachmentReference
                       0 -- attachment ix
                       Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                   )
                   Vector.empty -- resolve attachments
                   Nothing      -- depth stencil attachments
                   Vector.empty -- preserve attachments
               )
               -- subpass dependencies
               ( Vector.singleton $ Vk.SubpassDependency
                   Vk.SUBPASS_EXTERNAL -- src subpass
                   0 -- dst subpass
                   Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT -- src stage mask
                   Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT -- dst stage mask
                   Vk.zero -- src access mask
                   Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT -- dst access mask
                   Vk.zero -- dependency flags
               )

    subpasses <-
      forM (zip [0..] $ renderPassInfoSubpasses renderPassInfo) $ \(subpassIx, subpassInfo) -> do
        mSetLayout <-
          case subpassInfoDescriptors subpassInfo of
            [] ->
              pure Nothing
            setLayoutBindings ->
              fmap Just <$> withVkDescriptorSetLayout (vkDevice ctx)
                $ Vk.DescriptorSetLayoutCreateInfo
                    ()
                    Vk.zero
                    (Vector.fromList $ setLayoutBindings)

        pipelineLayout <-
          withVkPipelineLayout (vkDevice ctx)
            $ Vk.PipelineLayoutCreateInfo
                Vk.zero
                -- set layouts
                -- TODO: multiple set layouts
                (case mSetLayout of
                   Nothing ->
                     Vector.empty
                   Just setLayout ->
                     Vector.singleton $ Resource.get setLayout
                )
                -- push constant ranges
                Vector.empty

        let
          pipelineCreateInfo = Vk.GraphicsPipelineCreateInfo () Vk.zero
            (fromIntegral $ length $ subpassInfoShaderStages subpassInfo)
            (fmap Vk.SomeStruct . Vector.fromList $ subpassInfoShaderStages subpassInfo)
            ( Just . Vk.SomeStruct
              $ Vk.PipelineVertexInputStateCreateInfo
                  ()
                  Vk.zero
                  (Vector.fromList $ subpassInfoVertexBindings subpassInfo)
                  (Vector.fromList $ subpassInfoVertexAttributes subpassInfo)
            )
            ( Just
              $ Vk.PipelineInputAssemblyStateCreateInfo
                  Vk.zero
                  Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST -- Topology
                  False -- Primitive restart
            )
            Nothing -- tesselation
            ( Just . Vk.SomeStruct
              $ Vk.PipelineViewportStateCreateInfo
                  ()
                  Vk.zero
                  -- Viewports (dynamic so empty)
                  1
                  Vector.empty
                  -- Scissors (dynamic so empty)
                  1
                  Vector.empty
            )
            ( Just . Vk.SomeStruct
             $ Vk.PipelineRasterizationStateCreateInfo
                 ()
                 Vk.zero
                 False -- Depth clamp
                 False -- Rasterizer discard
                 Vk.POLYGON_MODE_FILL -- Polygon mode
                 Vk.CULL_MODE_NONE -- Cull mode
                 Vk.FRONT_FACE_CLOCKWISE -- Facing
                 False -- Depth bias
                 0 -- Depth bias constant factor
                 0 -- Depth bias clamp
                 0 -- Depth bias slope factor
                 1 -- Line width
            )
            ( Just . Vk.SomeStruct
                $ Vk.PipelineMultisampleStateCreateInfo
                    ()
                    Vk.zero
                    Vk.SAMPLE_COUNT_1_BIT -- Number of samples
                    False -- Sample shading enable
                    1 -- Min sample shading
                    Vector.empty -- Sample mask
                    False -- Alpha to coverage
                    False -- Alpha to one
            )
            ( Just
                $ Vk.PipelineDepthStencilStateCreateInfo
                    Vk.zero
                    True -- depth test enable
                    True -- depth write enable
                    Vk.COMPARE_OP_LESS -- compare op
                    False -- bounds test
                    False -- stencil test enable
                    Vk.zero -- front stencil
                    Vk.zero -- back stencil
                    0 -- min depth
                    1 -- max depth
            )
            ( Just . Vk.SomeStruct
                $ Vk.PipelineColorBlendStateCreateInfo
                    ()
                    Vk.zero
                    False -- Logic op enable
                    Vk.LOGIC_OP_COPY -- logic op
                    1 -- attachment count
                    -- attachments
                    ( Vector.singleton
                      $ Vk.PipelineColorBlendAttachmentState
                          False -- blend enable
                          Vk.zero -- src color blend factor
                          Vk.zero -- dst color blend factor
                          Vk.zero -- color blend op
                          Vk.zero -- src alpha blend factor
                          Vk.zero -- dst alpha blend factor
                          Vk.zero -- alpha blend op
                          -- color write mask
                          (Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT)
                    )
                    (0, 0, 0, 0) -- Blend constants
            )
            ( Just
                $ Vk.PipelineDynamicStateCreateInfo
                    Vk.zero
                    (Vector.fromList [ Vk.DYNAMIC_STATE_VIEWPORT
                                     , Vk.DYNAMIC_STATE_SCISSOR
                                     ]
                    )
            )
            (Resource.get pipelineLayout)
            (Resource.get rp)
            subpassIx -- subpass index
            Vk.NULL_HANDLE -- pipeline handle (inheritance)
            (-1) -- pipeline index (inheritance)

        pipeline <-
          fmap Vector.head <$> withVkGraphicsPipelines
            (vkDevice ctx)
            Vk.NULL_HANDLE
            (Vector.singleton pipelineCreateInfo)

        pure $ Subpass
          <$> pipeline
          <*> pipelineLayout
          <*> sequence mSetLayout
          <*> (pure $ subpassInfoDraw subpassInfo)

    pure $ RenderPass <$> rp <*> sequence subpasses

  frameDatas <-
    forM (Vector.toList $ vkImageViews swapchain) $ \iv -> do
      frameRenderPasses <- fmap sequence <$> forM renderPasses $ \rp -> do
        framebuffer <- withVkFramebuffer (vkDevice ctx)
          $ Vk.FramebufferCreateInfo
            ()
            Vk.zero
            (renderPass $ Resource.get rp) -- render pass
            (Vector.singleton iv) -- attachments
            -- dimensions
            (Extent2D.width $ vkExtent swapchain)
            (Extent2D.height $ vkExtent swapchain)
            1 -- layers
        pure $ (,) <$> framebuffer <*> rp

      -- TODO: We only need one descriptor pool for each frame in flight, not
      -- each swapchain image
      let descriptorPoolSizes = getDescriptorPoolSizes renderPassInfos

      descriptorPool <- withVkDescriptorPool (vkDevice ctx)
        $ Vk.DescriptorPoolCreateInfo
            ()
            Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
            -- TODO: One set for each frame in flight
            1 -- max sets that can be allocated
            descriptorPoolSizes

      pure $ FrameData <$> frameRenderPasses <*> descriptorPool

  pure $ sequence frameDatas

recordFrameData
  :: MonadIO m
  => Vk.CommandBuffer
  -> Swapchain
  -> FrameData
  -> m ()
recordFrameData cmdBuffer swapchain frameData = do
  forM_ (frameRenderPasses frameData) $ \(framebuffer, rp) -> do
    let
      clearValues = Vector.fromList [Vk.Color $ Vk.Float32 (254/255) (243/255) (215/255) 0]
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

    forM_ (renderPassSubpasses rp) $ \subpass -> do
      let draw = subpassDraw subpass
      Vk.cmdBindPipeline
        cmdBuffer
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        (subpassPipeline subpass)

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


      unless (isJust $ drawDescriptors draw) $
        let descBuffer = (drawDescriptor draw)
        set <- withVkDescriptorSet
          _device
          $ Vk.DescriptorSetAllocateInfo
              ()
              _pool
              (Vector.fromList $ subpassDescriptorSetLayout subpass)
        Vk.cmdBindDescriptorSets
          cmdBuffer
          Vk.PIPELINE_BIND_POINT_GRAPHICS
          (subpassPipelineLayout subpass)
          0 -- first set
          (Vector.fromList $ drawDescriptorSets draw)
          mempty -- dynamic offsets

      case drawCall draw of
        (IndexedDraw (DrawCallIndexed indexCount instanceCount firstIndex vertexOffset firstInstance)) ->
          Vk.cmdDrawIndexed
            cmdBuffer indexCount instanceCount firstIndex vertexOffset firstInstance
        (PrimitiveDraw (DrawCallPrimitive firstVertex firstInstance instanceCount vertexCount)) ->
          Vk.cmdDraw
            cmdBuffer vertexCount instanceCount firstVertex firstInstance

    Vk.cmdEndRenderPass cmdBuffer

getDescriptorPoolSizes
  :: [RenderPassInfo]
  -> Vector Vk.DescriptorPoolSize
getDescriptorPoolSizes rps =
  let
    poolSizeMap :: Map Vk.DescriptorType Word32
    poolSizeMap =
      flip foldMap rps $ \rp ->
        flip foldMap (renderPassInfoSubpasses rp) $ \sp ->
          flip foldMap (subpassInfoDescriptors sp) $ \binding ->
            Map.singleton binding.descriptorType
                          binding.descriptorCount

    poolSizes :: Vector Vk.DescriptorPoolSize
    poolSizes =
      Vector.fromList
      $ Map.toList poolSizeMap <&> uncurry Vk.DescriptorPoolSize
  in
    poolSizes
