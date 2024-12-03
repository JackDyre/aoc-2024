module Day03 (run03) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Ix (Ix (inRange))

run03 :: String -> (Int, Int)
run03 = runPt1 &&& runPt2
  where
    runPt1 = search
    runPt2 = search . donts

    search [] = 0
    search txt@(_ : cs) = case match txt of
      Just (n1, n2) -> n1 * n2 + search cs
      Nothing -> search cs
    donts = donts' True
    donts' _ [] = []
    donts' state txt@(c : cs)
      | take 4 txt == "do()" = donts' True (drop 4 txt)
      | take 7 txt == "don't()" = donts' False (drop 7 txt)
      | state = c : donts' state cs
      | otherwise = donts' state cs
    match txt = do
      txt' <- text "mul(" txt
      (txt'', n1) <- num txt'
      txt''' <- text "," txt''
      (txt'''', n2) <- num txt'''
      _ <- text ")" txt''''
      return (n1, n2)
    text str txt = do
      guard $ take (length str) txt == str
      return $ drop (length str) txt
    num txt = do
      guard $ inRange (1, 3) (length $ takeWhile isDigit txt)
      return (dropWhile isDigit txt, (read @Int) $ takeWhile isDigit txt)
