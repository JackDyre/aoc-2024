module Day01 (run01) where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.List (intersect, sort, transpose)

run01 :: String -> (Int, Int)
run01 = (runPt1 &&& runPt2) . parseInp
  where
    runPt1 = sum . map (abs . liftM2 (-) head last) . transpose . map sort
    runPt2 = sum . liftM2 intersect last head

    parseInp = transpose . map (map (read @Int) . words) . lines
