module Rort.Render.Renderer ( Renderer
                            , Handle
                            , create
                            , mkPipeline
                            , mkShader
                            ) where

import Control.Monad.Trans.Resource (MonadResource)
import Rort.Vulkan.Context (VkContext)
import Rort.Render.Types (PipelineInfo, Pipeline, ShaderInfo, Shader, Handle(..))
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM as STM

data Renderer = Renderer { pipelines :: TVar [PipelineInfo]
                         , shaders   :: TVar [ShaderInfo]
                         }

create :: MonadResource m => VkContext -> m Renderer
create ctx = do
  initialPipelines <- liftIO $ newTVarIO []
  initialShaders <- liftIO $ newTVarIO []

  pure $ Renderer initialPipelines initialShaders

mkPipeline :: MonadResource m => Renderer -> PipelineInfo -> m (Handle Pipeline)
mkPipeline r pipelineInfo = liftIO . STM.atomically $
  STM.stateTVar (pipelines r) $ \ps ->
    (Handle $ length ps, ps <> [pipelineInfo])

mkShader :: MonadResource m => Renderer -> ShaderInfo -> m (Handle Shader)
mkShader r shaderInfo = liftIO . STM.atomically $
  STM.stateTVar (shaders r) $ \ps ->
    (Handle $ length ps, ps <> [shaderInfo])
