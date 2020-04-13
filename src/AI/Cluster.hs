module AI.Cluster where


import Data.Vector
import qualified Data.Vector.Unboxed as Unboxed

import AI.Cluster.LloydsAlgorithm
--import AI.Cluster.Initialise
import AI.Cluster.Types


data InitialiseOptions = 
    RGC
  | RP
  | RUNFP
  | SIMFP
  | KMPP
  | GREP

kMeansCluster
  :: (HasDistance a, Ord a, Unboxed.Unbox a)
  => (Unboxed.Vector a -> a)
  -> Unboxed.Vector a
  -> ClusterOpts
  -> Vector (Unboxed.Vector a)
kMeansCluster kMeans inputs opts =
  let
  --   initial  = initialAssignment kMedians inputs opts
     initial  = undefined
     final    = lloydMeans kMeans inputs opts initial
  in
    assignment final
