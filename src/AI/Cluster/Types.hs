{-# LANGUAGE ScopedTypeVariables #-}
module AI.Cluster.Types where

import           Data.Monoid                 (Sum)
import           Data.Vector                 (Vector)

class HasDistance a where
  dist :: a -> a -> Sum Double

  squaredDist :: a -> a -> Sum Double
  squaredDist a b = (^ (2 :: Int)) <$> (dist a b)


data Cluster a = Cluster
  { clusterPoints :: Vector (Int, a)
  , assignment    :: Vector (Vector a)
  }

data ClusterOpts = ClusterOpts
  { numberOfClusters   :: {-# UNPACK #-} !Int
  , numberOfIterations :: {-# UNPACK #-} !Int
  }


