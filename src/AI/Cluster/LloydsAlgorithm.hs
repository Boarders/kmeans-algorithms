{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module AI.Cluster.LloydsAlgorithm where

import           Control.Parallel.Strategies hiding (parMap)
import           Data.Foldable
import           Data.Monoid                 (Sum)
import           Data.Ord                    (comparing)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Mutable         as MVector
import qualified Data.Vector.Unboxed         as Unboxed
import           VectorBuilder.Builder       (Builder)
import qualified VectorBuilder.Builder       as VB
import           VectorBuilder.Vector        (build)

import           AI.Cluster.Types
import           AI.Utility



lloydMeans
  :: forall a . (HasDistance a, Eq a, Unboxed.Unbox a)
  => (Unboxed.Vector a -> a)
  -> Unboxed.Vector a
  -> ClusterOpts
  -> Cluster a
  -> Cluster a
lloydMeans kMeans inputs ClusterOpts{..} = go 0
  where
    go :: Int -> Cluster a -> Cluster a
    go iter currClusters | iter >= numberOfIterations = currClusters
    go iter currClusters =
      let
        currAssignment :: Vector (Unboxed.Vector a)
        currAssignment  = assignment currClusters

        newClusterPoints :: Boxed.Vector (Int, a)
        newClusterPoints = Boxed.indexed (fmap kMeans currAssignment)

        newClusters :: Cluster a
        newClusters = assignClusters newClusterPoints inputs

        newAssignment :: Vector (Unboxed.Vector a)
        newAssignment = assignment newClusters
      in
        if currAssignment == newAssignment then
             newClusters
        else
          go (iter + 1) newClusters



assignClusters
  :: forall a . (HasDistance a, Unboxed.Unbox a)
  => Vector (Int, a)
  -> Unboxed.Vector a
  -> Cluster a
assignClusters clusterPoints inputs = Cluster{..}
  where
    numberOfClusters = length clusterPoints

    assignment = build <$> assignmentBuilder

    assignmentBuilder :: Vector (Builder a)
    assignmentBuilder =
      let
        numberOfInputs   = Unboxed.length inputs

        chunkedInputs :: Vector (Unboxed.Vector a)
        chunkedInputs =
          chunksOf (max (floor @Double . log . fromIntegral $ numberOfInputs) 1000) inputs
        partialClusters :: Vector (Vector (Builder a))
        partialClusters = parFmap rpar assign chunkedInputs

        combinedClusters :: Vector (Builder a)
        combinedClusters = combine numberOfClusters partialClusters
      in
        combinedClusters

    assign :: Unboxed.Vector a -> Vector (Builder a)
    assign inputVector = Boxed.create $ do
      mVec <- MVector.replicate numberOfClusters VB.empty
      let
        addPoint a = do
           let clusterLabel = nearest a
           MVector.modify mVec (<> (VB.singleton a)) clusterLabel


        nearest :: a -> Int
        nearest a = fst $ minimumBy (comparing (clusterDist a)) clusterPoints

        clusterDist :: a -> ((Int, a) -> Sum Double)
        clusterDist inp  (_, clusterPoint) = dist inp clusterPoint

      Unboxed.mapM_ addPoint inputVector
      pure mVec

    combine :: Int -> Vector (Vector (Builder a)) -> Vector (Builder a)
    combine l = foldr (Boxed.zipWith (<>)) (Boxed.replicate l mempty)



