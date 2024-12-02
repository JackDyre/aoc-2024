module Day02 where

import Control.Arrow((&&&))
import Data.List (tails)

run02 :: String -> (Int, Int)
run02 = run02p1 &&& run02p2

run02p1 :: String -> Int
run02p1 = length . filter f . lines

f :: String -> Bool
f inp = g $ diffs $ rNums inp

g :: [Int] -> Bool
g ds
  | signum (head ds) == 1 = (maximum ds <= 3) && (minimum ds >= 1)
  | signum (head ds) == -1 = (maximum ds <= -1) && (minimum ds >= -3)
  | otherwise = False

diffs :: [Int] -> [Int]
diffs = map ((\xs -> last xs - head xs) . take 2) . filter ((2 <=) .  length) . tails

rNums :: String -> [Int]
rNums = map read . words

mutate :: [a] -> [[a]]
mutate xs = xs  : muts
    where
        muts = map (`rAt` xs) [0..length xs - 1]

rAt :: (Eq t, Num t) => t -> [a] -> [a]
rAt _ [] = []
rAt 0 (_:xs) = xs
rAt n (x:xs) = x : rAt (n-1) xs


run02p2 :: String -> Int
run02p2 = length . filter h . lines

h :: String -> Bool
h = any (g . diffs) . mutate . rNums