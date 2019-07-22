{-# LANGUAGE RecordWildCards #-}
module AI.Cluster.Scoring where

import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.Monoid                 (Sum)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V

import AI.Cluster.Types
import AI.Utility


scoreClusters
  :: (HasDistance a)
  => Cluster a -> Sum Double
scoreClusters Cluster{..} =
  let
    clusterCents    = fmap snd clusterPoints
    clusterPairs    = V.zip clusterCents assignment
    scoredIndividualClusters = parFmap rpar scoreCluster clusterPairs
  in
    fold scoredIndividualClusters


scoreCluster
  :: (HasDistance a)
  => (a, Vector a) -> Sum Double
scoreCluster (clusterPt, cluster) = foldMap (squaredDist clusterPt) cluster
