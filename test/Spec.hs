import ProcessInt (processInterpolation)
import System.IO.Silently (capture)
import Test.HUnit

testProcessInterpolationLinear1 :: Test
testProcessInterpolationLinear1 = TestCase $ do
  let points = [(0, 0), (1.571, 1)]
  let step = 1.0
  let method = 1

  (output, _) <- capture (processInterpolation method points step)

  let expectedOutput = "Результат линейной интерполяции:\nx: 0.0 1.0 2.0\ny: 0.0 0.64 1.27\n\n"

  assertEqual "Linear interpolation output doesn't match" expectedOutput output

testProcessInterpolationLinear2 :: Test
testProcessInterpolationLinear2 = TestCase $ do
  let points = [(1.571, 1), (3.142, 0)]
  let step = 1.0
  let method = 1

  (output, _) <- capture (processInterpolation method points step)

  let expectedOutput = "Результат линейной интерполяции:\nx: 1.57 2.57 3.57\ny: 1.0 0.36 -0.27\n\n"

  assertEqual "Linear interpolation output doesn't match" expectedOutput output

testProcessInterpolationLagrange1 :: Test
testProcessInterpolationLagrange1 = TestCase $ do
  let points = [(0.0, 0.0), (1.571, 1), (3.142, 0)]
  let step = 1.0
  let method = 2

  (output, _) <- capture (processInterpolation method points step)

  let expectedOutput = "Результат интерполяции Лагранжа:\nx: 0.0 1.0 2.0 3.0 4.0\ny: 0.0 0.87 0.93 0.17 -1.39\n\n"

  assertEqual "Linear interpolation output doesn't match" expectedOutput output

testProcessInterpolationLagrange2 :: Test
testProcessInterpolationLagrange2 = TestCase $ do
  let points = [(1.571, 1), (3.142, 0), (4.712, -1)]
  let step = 1.0
  let method = 2

  (output, _) <- capture (processInterpolation method points step)

  let expectedOutput = "Результат интерполяции Лагранжа:\nx: 1.57 2.57 3.57 4.57 5.57\ny: 1.0 0.36 -0.27 -0.91 -1.55\n\n"

  assertEqual "Linear interpolation output doesn't match" expectedOutput output

testProcessInterpolationBoth :: Test
testProcessInterpolationBoth = TestCase $ do
  let points = [(1.571, 1), (3.142, 0), (4.712, -1)]
  let step = 1.0
  let method = 3

  (output, _) <- capture (processInterpolation method points step)

  let expectedOutput = "Результат линейной интерполяции:\nx: 3.14 4.14 5.14\ny: 0.0 -0.64 -1.27\nРезультат интерполяции Лагранжа:\nx: 1.57 2.57 3.57 4.57 5.57\ny: 1.0 0.36 -0.27 -0.91 -1.55\n\n"

  assertEqual "Linear interpolation output doesn't match" expectedOutput output

main :: IO ()
main = runTestTT tests >>= print

tests :: Test
tests =
  TestList
    [ testProcessInterpolationLinear1,
      testProcessInterpolationLinear2,
      testProcessInterpolationLagrange1,
      testProcessInterpolationLagrange2,
      testProcessInterpolationBoth
    ]
