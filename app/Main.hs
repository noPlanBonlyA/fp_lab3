module Main (main, processInterpolation) where

import ProcessInt (processInterpolation)
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [methodArg, stepArg] -> do
      let method = readInput methodArg
      let step = readInputStep stepArg
      case (method, step) of
        (Just m, Just s) -> do
          putStrLn "Введите начальную точку (x, y):"
          startPointInput <- getLine
          let startPoint = readInputPoint startPointInput
          case startPoint of
            Just sp -> loop m [sp] s
            Nothing -> putStrLn "Некорректная начальная точка."
        _ -> putStrLn "Некорректные аргументы. Используйте: <метод> <шаг>"
    _ -> putStrLn "Неверное количество аргументов. Используйте: <метод> <шаг>"

readInput :: String -> Maybe Int
readInput input = readMaybe input :: Maybe Int

readInputStep :: String -> Maybe Double
readInputStep input = readMaybe input :: Maybe Double

readInputPoint :: String -> Maybe (Double, Double)
readInputPoint input = case words input of
  [xStr, yStr] -> do
    x <- readMaybe xStr :: Maybe Double
    y <- readMaybe yStr :: Maybe Double
    return (x, y)
  _ -> Nothing

loop :: Int -> [(Double, Double)] -> Double -> IO ()
loop method points step = do
  putStrLn "Введите точку (x, y) или 'exit' для выхода:"
  input <- getLine
  if input == "exit"
    then return ()
    else do
      let newPoint = readInputPoint input
      case newPoint of
        Just np -> do
          let newPoints = points ++ [np]
          processInterpolation method newPoints step
          loop method newPoints step
        Nothing -> putStrLn "Некорректный формат точки." >> loop method points step
