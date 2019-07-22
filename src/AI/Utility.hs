module AI.Utility where

import           Control.Parallel.Strategies hiding (parMap)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as V
import           VectorBuilder.Builder       (Builder)
import qualified VectorBuilder.Builder       as VB
import           VectorBuilder.Vector        (build)

chunksOf :: Int -> Vector a -> Vector (Vector a)
chunksOf chunk vec =
  let
    len = length vec
  in
    build $ go len chunk vec
  where
    go :: Int -> Int -> Vector a -> Builder (Vector a)
    go l c v =
      if l >= c then
        VB.singleton (V.take c v) <> go (l - c) c (V.drop c v)
      else
        VB.singleton v


parFmap :: Traversable t => Strategy b -> (a -> b) -> t a -> t b
parFmap strat f = (`using` parTraversable strat) . fmap f
