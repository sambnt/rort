module Rort.Util.Gltf where

import qualified Text.GLTF.Loader as Gltf
import Data.Word (Word16)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Linear (V3)
import qualified Linear
import Data.Maybe (fromMaybe)
import Data.Foldable (toList, foldl')
import Rort.Render.Types (DrawCallIndexed(..), DrawCall (..))

data Mesh = Mesh { meshDrawCall :: [DrawCall]
                 }

data Scene = Scene { sceneMeshes :: [Mesh]
                   }

processMeshPrimitives
  :: (Word16, [Float], [Word16], [DrawCall])
  -> Gltf.MeshPrimitive
  -> (Word16, [Float], [Word16], [DrawCall])
processMeshPrimitives (curIx, accBuffer, accIndex, drawCalls) meshPrim =
  let
    drawCall =
      IndexedDraw $ DrawCallIndexed
        { drawCallIndexedIndexCount = fromIntegral $ length $ Gltf.meshPrimitiveIndices meshPrim
        , drawCallIndexedInstanceCount = 1
        , drawCallIndexedFirstIndex = fromIntegral $ length accIndex
        , drawCallIndexedVertexOffset = 0
        , drawCallIndexedFirstInstance = 0
        }

    (curIx', accBuffer', accIndex') =
      foldl'
      (\(ixAcc, bufferAcc, indexAcc) ix ->
          let
            pos      = getPosition (Gltf.meshPrimitivePositions meshPrim) ix
            texCoord = getTexCoord (Gltf.meshPrimitiveTexCoords meshPrim) ix
            normal   = getNormal (Gltf.meshPrimitiveNormals meshPrim) ix
            newVert  = toList pos <> toList texCoord <> toList normal
          in
            (ixAcc + 1, bufferAcc <> newVert, indexAcc <> [ixAcc])
      )
      (curIx, accBuffer, accIndex) (Gltf.meshPrimitiveIndices meshPrim)
  in
    (curIx', accBuffer', accIndex', drawCalls <> [drawCall])

processMesh
  :: (Word16, [Float], [Word16], [Mesh])
  -> Gltf.Mesh
  -> (Word16, [Float], [Word16], [Mesh])
processMesh (accIx, accBuffer, accIndex, meshes) mesh =
  let
    (accIx', accBuffer', accIndex', drawCalls) =
      foldl'
        processMeshPrimitives
        (accIx, accBuffer, accIndex, [])
        (Gltf.meshPrimitives mesh)
  in
    (accIx', accBuffer', accIndex', meshes <> [Mesh drawCalls])

processGltf
  :: (Word16, [Float], [Word16], [Scene])
  -> Gltf.Gltf
  -> (Word16, [Float], [Word16], [Scene])
processGltf (accIx, accBuffer, accIndex, scenes) mesh =
  let
    (accIx', accBuffer', accIndex', meshes) =
      foldl' processMesh (accIx, accBuffer, accIndex, []) (Gltf.gltfMeshes mesh)
  in
    (accIx', accBuffer', accIndex', scenes <> [Scene meshes])

processGltf' :: Gltf.Gltf -> ([Float], [Word16], Scene)
processGltf' gltf =
  let
    (_, bufferData, indexData, scenes) = processGltf (0, [], [], []) gltf
  in
    (bufferData, indexData, head scenes)

getPosition :: Vector (V3 Float) -> Word16 -> V3 Float
getPosition = getIndex (Linear.V3 0 0 0)

getNormal :: Vector (V3 Float) -> Word16 -> V3 Float
getNormal   = getIndex (Linear.V3 0 0 0)

getTexCoord :: Vector (Linear.V2 Float) -> Word16 -> Linear.V2 Float
getTexCoord = getIndex (Linear.V2 0 0)

getIndex :: a -> Vector a -> Word16 -> a
getIndex def ns ix =
  fromMaybe def (ns Vector.!? fromIntegral ix)
