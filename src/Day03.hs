module Day03 (run03) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Ix (Ix (inRange))
import Data.List (isPrefixOf)

run03 :: String -> (Int, Int)
run03 = runPt1 &&& runPt2
  where
    runPt1 = search
    runPt2 = search . donts

    search [] = 0
    search xs = search (tail xs) + maybe 0 (uncurry (*)) (match xs)
    donts = donts' True
    donts' _ [] = []
    donts' state txt@(c : cs)
      | "do()" `isPrefixOf` txt = donts (drop 4 txt)
      | "don't()" `isPrefixOf` txt = donts' False (drop 7 txt)
      | state = c : donts cs
      | otherwise = donts' state cs
    match txt = do
      (txt', n1) <- textP "mul(" txt >>= numP
      (txt'', n2) <- textP "," txt' >>= numP
      _ <- textP ")" txt''
      return (n1, n2)
    textP str txt =
      guard (str `isPrefixOf` txt)
        >> return (drop (length str) txt)
    numP txt = do
      let (digs, rest) = span isDigit txt
      guard $ inRange (1, 3) (length digs)
      return (rest, read @Int digs)
