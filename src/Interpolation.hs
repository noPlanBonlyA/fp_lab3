module Interpolation (linearInterpolation, lagrangeInterpolation) where

import PointsGenerator (generatePoints)

linearInterpolation :: (Double, Double) -> (Double, Double) -> Double -> [(Double, Double)]
linearInterpolation (x1, y1) (x2, y2) step
  | step <= 0 = error "Step must be greater than 0"
  | otherwise = [(x, fromIntegral (round (interpolate x * 100) :: Integer) / 100) | x <- generatePoints x1 x2 step]
  where
    interpolate x = y1 + (x - x1) * (y2 - y1) / (x2 - x1)

lagrangeInterpolation :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double -> [(Double, Double)]
lagrangeInterpolation (x0, y0) (x1, y1) (x2, y2) step
  | step <= 0 = error "Step must be greater than 0"
  | otherwise = [(x, fromIntegral (round (interpolate x * 100) :: Integer) / 100) | x <- generatePoints x0 x2 step]
  where
    interpolate x =
      y0 * ((x - x1) * (x - x2)) / ((x0 - x1) * (x0 - x2))
        + y1 * ((x - x0) * (x - x2)) / ((x1 - x0) * (x1 - x2))
        + y2 * ((x - x0) * (x - x1)) / ((x2 - x0) * (x2 - x1))
