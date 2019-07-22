module AI.Cluster where

import AI.Cluster.LloydsAlgorithm
import AI.Cluster.Initialise
import Data.Vector

import AI.Cluster.Types


data InitialiseOptions = 
    RGC
  | RP
  | RUNFP
  | SIMFP
  | KMPP
  | GREP

kMeansCluster
  :: (HasDistance a, Ord a)
  => (Vector a -> a)
  -> Vector a
  -> ClusterOpts
  -> Vector (Vector a)
kMeansCluster kMedians inputs opts =
  let
     initial  = initialAssignment kMedians inputs opts
     final    = lloydMedians kMedians inputs opts initial
  in
    assignment final
