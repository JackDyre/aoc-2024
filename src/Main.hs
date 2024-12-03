module Main where

import Day01 (run01)
import Day02 (run02)
import Day03 
import Test (runTest)

main :: IO ()
main = do
  runTest "Day01 Test Case" run01 "inputs/testCaseDay01.txt" (11, 7)
  runTest "Day01" run01 "inputs/day01.txt" (2904518, 18650129)

  runTest "Day02 Test Case" run02 "inputs/testCaseDay02.txt" (2, 4)
  runTest "Day02" run02 "inputs/day02.txt" (287, 354)

  runTest "Day03 Test Case" run03 "inputs/testCaseDay03.txt" (161, 48)
  runTest "Day03" run03 "inputs/day03.txt" (189600467, 107069718)
