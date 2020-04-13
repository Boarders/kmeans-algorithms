{-# LANGUAGE ScopedTypeVariables #-}

module AI.Utility where

import           Control.Parallel.Strategies hiding (parMap)
import           Data.Vector                 (Vector)
import qualified Data.Vector.Unboxed         as Unboxed
import           VectorBuilder.Builder       (Builder)
import qualified VectorBuilder.Builder       as Builder
import           VectorBuilder.Vector        (build)

chunksOf
  :: forall a . (Unboxed.Unbox a)
  => Int
  -> Unboxed.Vector a
  -> Vector (Unboxed.Vector a)
chunksOf chunk vec =
  let
    len = Unboxed.length vec
  in
    build $ go len chunk vec
  where
    go :: Int -> Int -> Unboxed.Vector a -> Builder (Unboxed.Vector a)
    go l c v | l < c = Builder.singleton v
    go l c v = Builder.singleton (Unboxed.take c v) <> go (l - c) c (Unboxed.drop c v)




parFmap :: Traversable t => Strategy b -> (a -> b) -> t a -> t b
parFmap strat f = (`using` parTraversable strat) . fmap f
