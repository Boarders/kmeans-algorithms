{-# LANGUAGE RecordWildCards #-}
module AI.Cluster.Scoring where

import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.Monoid                 (Sum)
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Unboxed         as Unboxed

import AI.Cluster.Types
import AI.Utility


scoreClusters
  :: (HasDistance a, Unboxed.Unbox a)
  => Cluster a
  -> Sum Double
scoreClusters Cluster{..} =
  let
    clusterCents             = fmap snd clusterPoints
    clusterPairs             = Boxed.zip clusterCents assignment
    scoredIndividualClusters = parFmap rpar scoreCluster clusterPairs
  in
    fold scoredIndividualClusters


scoreCluster
  :: (HasDistance a, Unboxed.Unbox a)
  => (a, Unboxed.Vector a)
  -> Sum Double
scoreCluster (clusterPt, cluster)
  = Unboxed.foldr (\pt acc -> squaredDist clusterPt pt <> acc) mempty cluster
