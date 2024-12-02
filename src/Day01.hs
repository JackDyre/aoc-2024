module Day01(run01, run01p1, run01p2) where

import Control.Arrow((&&&))
import Data.List (sort, transpose)

run01 :: String -> (Int, Int)
run01 = (run01p1 &&& run01p2) . parseInp01

parseInp01 :: String -> [[Int]]
parseInp01 = transpose . map (map read . words) . lines

run01p1 :: [[Int]] -> Int
run01p1 = sum . map (\ x -> abs (head x - last x)) . transpose . map sort

run01p2 :: [[Int]] -> Int
run01p2 ns = sum $ map (\ n -> n * length (filter (n==) (last ns))) (head ns)