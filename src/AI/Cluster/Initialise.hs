{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE BangPatterns #-}



-- see this link: https://stats.stackexchange.com/questions/317493/methods-of-initializing-k-means-clustering/317498#317498

module AI.Cluster.Initialise where

import           Data.Vector                 (Vector, (!))
import qualified Data.Vector                 as V
import           Immutable.Shuffle           (shuffleM, sampleWithoutReplacement)
import           VectorBuilder.Builder       (Builder)
import qualified VectorBuilder.Builder       as VB
import           VectorBuilder.Vector        (build)
import           Control.Monad.Random.Class  
import Control.Monad.Primitive
import AI.Cluster.LloydsAlgorithm
import System.Random.MWC.Probability (Prob, GenIO)
import Data.Coerce
import Data.Monoid (Sum(..))
import qualified System.Random.MWC.Probability as MWCP
import Control.Monad.State.Strict
import Data.Foldable
import Data.List (findIndex)


import AI.Cluster.Types
import AI.Utility

randomInitialAssignment
  :: forall a
  .  (Vector a -> a)
  -> Vector a
  -> ClusterOpts
  -> IO (Cluster a)
randomInitialAssignment kMedians inputs opts = do
  shuffleInputs <- shuffleM inputs
  pure $ initialAssignment kMedians shuffleInputs opts

initialAssignment
  :: forall a
  . (Vector a -> a)
  -> Vector a
  -> ClusterOpts
  -> Cluster a
initialAssignment kMedians inputs ClusterOpts{..} =
  let
    inputSize = length inputs
    chunkSize = ceiling $ fromIntegral @_ @Double inputSize / fromIntegral @_ @Double numberOfClusters
    chunks        = chunksOf chunkSize inputs
    clusterPoints = kMedians <$> chunks
  in
    Cluster
    { clusterPoints = V.indexed clusterPoints
    , assignment    = chunks
    }


-- | RP
-- Randomly selected points
rp :: forall a m . (HasDistance a, MonadRandom m, PrimMonad m)
  => (Vector a -> a)
  -> Vector a
  -> ClusterOpts
  -> m (Cluster a)
rp _ inputs ClusterOpts{..} = do
  kSample <- sampleWithoutReplacement numberOfClusters inputs
  let newClusters = V.indexed kSample
  pure $ assignClusters newClusters inputs

-- | KMPP
-- K-means++
kmpp :: forall a . (HasDistance a)
  => (Vector a -> a)
  -> Vector a
  -> ClusterOpts
  -> IO (Cluster a)
kmpp _ inputs ClusterOpts{..} = do
      gen <- MWCP.createSystemRandom
      let numberOfInputs = length inputs

      firstSampleInd <- getRandomR (0,numberOfInputs - 1)
      let firstSample = inputs ! firstSampleInd
      let firstClusterPt = (firstSampleInd, firstSample)
      let distsFromFirstSample = fmap (coerce $ dist firstSample) inputs
      let initialKMPP = KMeansPlusPlus distsFromFirstSample (VB.singleton firstClusterPt)
      KMeansPlusPlus{..} <- go gen (numberOfClusters - 1) initialKMPP
      let finalClusterPts = build _clusterPts
      pure $ assignClusters finalClusterPts inputs
  where  
    go :: GenIO -> Int -> KMeansPlusPlus a -> IO (KMeansPlusPlus a )
    go _ 0 km = pure km
    go g n ~KMeansPlusPlus{..} = do
      sampleInd <- MWCP.sample (categorical _distances) g
      let sample = inputs ! sampleInd
      let clusterPt = (sampleInd, sample)
      let distsFromSample = fmap (coerce $ dist sample) inputs
      let newDists  = V.zipWith min distsFromSample _distances
      let newClusts = _clusterPts <> (VB.singleton clusterPt)
      let newKmeans = KMeansPlusPlus newDists newClusts
      go g (n - 1) newKmeans
        


data KMeansPlusPlus a = KMeansPlusPlus
  { _distances     :: Vector Double
  , _clusterPts    :: Builder (Int, a)
  }


multinomial :: (Foldable f, PrimMonad m) => Int -> f Double -> Prob m [Int]
multinomial n ps  = do
  let (scan, tot) = cumulative @Double (toList ps)
  replicateM n $ do
    z <- MWCP.uniformR (0,tot)
    case findIndex (> z) scan of
      Just g  -> return g
      Nothing -> error "mwc-probability: invalid probability vector"
{-# INLINABLE multinomial #-}

categorical :: (Foldable f, PrimMonad m) => f Double -> Prob m Int
categorical ps = do
  xs <- multinomial 1 ps
  pure (head xs)


cumulative :: forall a . Num a => [a] -> ([a], a)
cumulative as = let acc = scanl1 (+) as in (acc, sum as)
