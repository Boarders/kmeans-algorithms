{-# LANGUAGE BlockArguments #-}
module Main where

import           AI.Cluster.Types
import           Control.Lens
import           Control.Monad.Primitive                (RealWorld)
import           Data.Colour.SRGB                       (Colour (..), sRGB24)
import qualified Data.Vector                            as Boxed
import qualified Data.Vector.Unboxed                    as Unboxed
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import qualified Graphics.Rendering.Chart.Easy          as Chart
import qualified System.Random.MWC                      as MWC
import qualified System.Random.MWC.Distributions        as MWC

main :: IO ()
main =
  do
    norm1 <- generateNormalDist (1  , 1) (1  , 1  ) 3000
    norm2 <- generateNormalDist (4  , 3) (1.5, 1.5) 3000
    norm3 <- generateNormalDist (4.5,-2) (1  , 1  ) 3000
    Cairo.toFile Chart.def "gaussian.png"
      do
        Chart.layout_title .= "Gaussian Mixture"
        Chart.layout_background . Chart.fill_color .= beige
        Chart.layout_legend .= Nothing
        Chart.setColors (cycle [red, blue, green])
        let circ = Chart.PointShapeCircle
        Chart.setShapes (repeat circ)
        Chart.plot $ renderPoints norm1
        Chart.plot $ renderPoints norm2
        Chart.plot $ renderPoints norm3

generateNormalDist
  :: (Double, Double)  -- ^ Mean
  -> (Double, Double)  -- ^ Variance
  -> Int               -- ^ Number of Points
  -> IO (Unboxed.Vector Point2)
generateNormalDist (meanX, meanY) (varX, varY) numPts =
  MWC.withSystemRandom \genX ->
    MWC.withSystemRandom \genY ->
      Unboxed.replicateM numPts $ genPoint genX genY
  where
    genPoint :: MWC.Gen RealWorld -> MWC.Gen RealWorld -> IO Point2
    genPoint genX genY =
      do
        normX <- MWC.normal meanX varX genX
        normY <- MWC.normal meanY varY genY
        pure $ Point2 normX normY

renderPoints :: Unboxed.Vector Point2 -> Chart.EC l (Chart.PlotPoints Double Double)
renderPoints vec =
  do
    let circ = Chart.PointShapeCircle
    Chart.setShapes (repeat circ)
    points <- Chart.points "clusters"
            . Unboxed.toList
            . Unboxed.map (\ (Point2 x y) -> (x, y))
            $ vec
    let points' = set (Chart.plot_points_style . Chart.point_radius) 1.25 points
    pure points'



red
  :: (Ord a, Floating a)
  => Chart.AlphaColour a
red = Chart.opaque $ sRGB24 200 50 5

green
  :: (Ord a, Floating a)
  => Chart.AlphaColour a
green = Chart.opaque $ sRGB24 130 200 5

blue
  :: (Ord a, Floating a)
  => Chart.AlphaColour a
blue = Chart.opaque $ sRGB24 30 90 255

beige
  :: (Ord a, Floating a)
  => Chart.AlphaColour a
beige = Chart.opaque $ sRGB24 245 240 215
