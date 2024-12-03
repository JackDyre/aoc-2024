module Main where

import Day01 (run01)
import Day02 (run02)
import Day03 (run03)
import Test (runTest)

main :: IO ()
main = do
  runTest "Day01" run01 "inputs/day01.txt" (2904518, 18650129)
  runTest "Day02" run02 "inputs/day02.txt" (287, 354)
  runTest "Day03" run03 "inputs/day03.txt" (189600467, 107069718)
