module Rort.Util.Wavefront where

import qualified Codec.Wavefront as Obj
import qualified Data.Vector as Vector
import Data.Map.Strict (Map)
import Data.Word (Word32)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

loadObjUnique :: FilePath -> IO ([Float], [Word32])
loadObjUnique filepath = do
  obj <- either error id <$> Obj.fromFile filepath

  let
    getFaceIndices :: [Obj.FaceIndex]
    getFaceIndices =
      flip foldMap (Obj.elValue <$> Obj.objFaces obj) $
        \(Obj.Face i1 i2 i3 iRest) -> [i1, i2, i3] <> iRest

    getVertex :: Obj.FaceIndex -> [Float]
    getVertex (Obj.FaceIndex locIx mTexIx _mNormIx) =
      let
        (Obj.Location x y z _w) = Obj.objLocations obj Vector.! (locIx - 1)
        (Obj.TexCoord r s _t) = case mTexIx of
          Just texIx -> Obj.objTexCoords obj Vector.! (texIx - 1)
          Nothing    -> Obj.TexCoord 0 0 0
      in
        [x, y, z, r, 1 - s]

    mkUniqueVertex
      :: Ord a
      => a -> (Map a Word32, [a], [Word32]) -> (Map a Word32, [a], [Word32])
    mkUniqueVertex vert (uniqVerts, vs, is) =
      let
        (uniqVerts', verts') =
          if Map.notMember vert uniqVerts
          then (Map.insert vert (fromIntegral $ length vs) uniqVerts, vs <> [vert])
          else (uniqVerts, vs)
      in
        (uniqVerts', verts', is <> [fromJust $ Map.lookup vert uniqVerts'])

    (_, verts, indis) =
      foldl (\acc faceIx -> mkUniqueVertex (getVertex faceIx) acc) mempty getFaceIndices

  pure (concat verts, indis)
