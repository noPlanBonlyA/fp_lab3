module PointsGenerator (generatePoints) where

generatePoints :: Double -> Double -> Double -> [Double]
generatePoints start end step = takeWhile (\x -> x <= end + step) $ map (\x -> fromIntegral (round (x * 100) :: Integer) / 100) [start, start + step ..]
