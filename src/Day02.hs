module Day02 (run02) where

import Control.Arrow ((&&&))
import Control.Monad (liftM2)
import Data.Ix (Ix (inRange))

run02 :: String -> (Int, Int)
run02 = (runPt1 &&& runPt2) . parseInp
  where
    runPt1 = length . filter safe
    runPt2 = length . filter (any safe . subs)

    parseInp = map (map (read @Int) . words) . lines
    safe = liftM2 (||) safe' (safe' . reverse)
    safe' = all (inRange (1, 3)) . (zipWith (-) =<< tail)
    subs [] = []
    subs (y : ys) = ys : map (y :) (subs ys)
