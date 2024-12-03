{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}
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
    search xs = maybe (search $ tail xs) ((+) (search $ tail xs) . uncurry (*)) (match xs)
    donts = donts' True
    donts' _ [] = []
    donts' state txt@(c : cs)
      | "do()" `isPrefixOf` txt = donts (drop 4 txt)
      | "don't()" `isPrefixOf` txt = donts' False (drop 7 txt)
      | state = c : donts cs
      | otherwise = donts' state cs
    match :: String -> Maybe (Int, Int)
    match txt = do
      (txt', n1) <- text "mul(" txt >>= num
      (txt'', n2) <- text "," txt' >>= num
      _ <- text ")" txt''
      return (n1, n2)
    text :: String -> String -> Maybe String
    text str txt = do
      guard $ and (zipWith (==) str txt)
      return $ drop (length str) txt
    num :: String -> Maybe (String, Int)
    num txt = do
      let (digs, rest) = span isDigit txt
      guard $ inRange (1, 3) (length digs)
      return (rest, read @Int digs)
