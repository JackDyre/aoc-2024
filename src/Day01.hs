module Day01 where

import Control.Arrow((&&&))
import Data.List (sort)
import Data.Monoid (Sum(..))
import Data.Bifunctor (bimap)

run01 :: String -> (Int, Int)
run01 = run01p1 &&& run01p2

run01p1 :: String -> Int
run01p1 = sum
   . uncurry (zipWith (\x y -> abs (read x - read y)))
   . bimap sort sort
   . parseInp

run01p2 :: String -> Int
run01p2 =
   (\(xs, ys) -> getSum $ foldMap (\x -> Sum $ read x * length (filter (x==) ys)) xs)
   . parseInp

parseInp :: String -> ([String], [String])
parseInp =
   (map (head . words) &&& map (last . words))
   . lines
