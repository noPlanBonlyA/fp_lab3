module ProcessInt (processInterpolation, printOutput) where

import Control.Monad (when)
import Interpolation (lagrangeInterpolation, linearInterpolation)

processInterpolation :: Int -> [(Double, Double)] -> Double -> IO ()
processInterpolation method points step = do
  let startPoint = points !! (length points - 2)
  let endPoint = last points

  let processLinear = do
        let output = linearInterpolation startPoint endPoint step
        putStrLn "Результат линейной интерполяции:"
        printOutput output

  let processLagrange = do
        let [p1, p2, p3] = drop (length points - 3) points
        let output = lagrangeInterpolation p1 p2 p3 step
        putStrLn "Результат интерполяции Лагранжа:"
        printOutput output

  case method of
    1 -> processLinear
    2 -> if length points >= 3 then processLagrange else putStrLn "Необходимо минимум 3 точки."
    3 -> do
      processLinear
      when (length points >= 3) processLagrange
  putStrLn ""

printOutput :: [(Double, Double)] -> IO ()
printOutput output = do
  let xValues = map fst output
  let yValues = map snd output
  putStrLn $ "x: " ++ unwords (map show xValues)
  putStrLn $ "y: " ++ unwords (map show yValues)
