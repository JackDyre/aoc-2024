module Day04 where

import Control.Arrow ((&&&))
import Control.Monad (join, liftM2)
import Data.List (transpose)
import Data.List.HT (shear)

run04 :: String -> (Int, Int)
run04 = runPt1 &&& runPt2
  where
    runPt1 = fun3 . lines
    runPt2 = fn2 . lines

fun3 :: [[Char]] -> Int
fun3 = sum . map fun . take 4 . (iterate rotMat)

fun :: [[Char]] -> Int
fun = liftM2 ((+)) (sum . map fun2) (sum . map fun2 . shear)

fun2 :: String -> Int
fun2 = length . filter (== "XMAS") . slidingWindow 4

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow _ [] = []
slidingWindow n l@(_ : xs) = if n <= length l then take n l : slidingWindow n xs else []

rotMat :: [[a]] -> [[a]]
rotMat = map reverse . transpose

sliding2d :: Int -> [[a]] -> [[[a]]]
sliding2d n = join . map (map transpose . slidingWindow n . transpose) . slidingWindow n

fn2 :: [[Char]] -> Int
fn2 = length . filter (any (fn3 . join) . (take 4 . (iterate rotMat))) . sliding2d 3

fn3 :: [Char] -> Bool
fn3 [m1, _, m2, _, a, _, s1, _, s2] | [m1, m2, a, s1, s2] == "MMASS" = True
fn3 _ = False
