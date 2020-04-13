{-# LANGUAGE ScopedTypeVariables #-}
module AI.Cluster.Types where

import           Data.Monoid                 (Sum)
import qualified Data.Vector as Boxed
import qualified Data.Vector.Unboxed as Unboxed

class HasDistance a where
  dist :: a -> a -> Sum Double

  squaredDist :: a -> a -> Sum Double
  {-# INLINE squaredDist #-}
  squaredDist a b = (^ (2 :: Int)) <$> dist a b


data Point2 = Point2
  { x2 :: {-# UNPACK #-} !Double
  , y2 :: {-# UNPACK #-} !Double
  }
  deriving (Ord, Eq)

data Point3 = Point3
  { x3 :: {-# UNPACK #-} !Double
  , y3 :: {-# UNPACK #-} !Double 
  , z3 :: {-# UNPACK #-} !Double 
  }
  deriving (Ord, Eq)


data Cluster a = Cluster
  { clusterPoints :: Boxed.Vector (Int, a)
  , assignment    :: Boxed.Vector (Unboxed.Vector a)
  }

data ClusterOpts = ClusterOpts
  { numberOfClusters   :: {-# UNPACK #-} !Int
  , numberOfIterations :: {-# UNPACK #-} !Int
  }


