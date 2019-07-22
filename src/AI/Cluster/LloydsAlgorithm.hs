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
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MVector
import           VectorBuilder.Builder       (Builder)
import qualified VectorBuilder.Builder       as VB
import           VectorBuilder.Vector        (build)

import AI.Cluster.Types
import AI.Utility



lloydMedians
  :: forall a . (HasDistance a, Eq a)
  => (Vector a -> a)
  -> Vector a
  -> ClusterOpts
  -> Cluster a
  -> Cluster a
lloydMedians kMedian inputs ClusterOpts{..} = go 0
  where
    go :: Int -> Cluster a -> Cluster a
    go iter currClusters =
      let
        currAssignmentV :: Vector (Vector a)
        currAssignmentV  = assignment currClusters
        newClusterPoints = V.indexed (kMedian <$> currAssignmentV)
        newCluster = assignClusters newClusterPoints inputs
        newAssignmentV :: Vector (Vector a)
        newAssignmentV = assignment newCluster
      in
        if iter >= numberOfIterations then
        currClusters
        else
           if currAssignmentV == newAssignmentV then
             newCluster
           else
             go (iter + 1) newCluster



assignClusters
  :: forall a . (HasDistance a)
  => Vector (Int, a)
  -> Vector a
  -> Cluster a
assignClusters clusterPoints inputs = Cluster{..}
  where
    numberOfClusters = length clusterPoints
    assignment = build <$> assignmentB
    assignmentB :: Vector (Builder a)
    assignmentB =
      let
        numberOfInputs   = length inputs

        chunkedInputs = chunksOf
                          (max (floor @Double . log . fromIntegral $ numberOfInputs) 10)
                          inputs
        partialClusters = parFmap rpar assign chunkedInputs
        combinedClusters = combine numberOfClusters partialClusters
      in
        combinedClusters

    assign :: Vector a -> Vector (Builder a)
    assign inputVector = V.create $ do
      vec <- MVector.replicate numberOfClusters VB.empty
      let
        addPoint a = do
           let clusterLabel = nearest a
           currCluster <- MVector.read vec clusterLabel
           MVector.write vec clusterLabel $ currCluster <> VB.singleton a

        nearest :: a -> Int
        nearest a = fst $ minimumBy (comparing (f a)) clusterPoints

        f :: a -> ((Int, a) -> Sum Double)
        f inp  (_, clusterPoint) = dist inp clusterPoint

      traverse_ addPoint inputVector
      pure vec

    combine :: Int -> Vector (Vector (Builder a)) -> Vector (Builder a)
    combine l = foldr (V.zipWith (<>)) (V.replicate l mempty)



