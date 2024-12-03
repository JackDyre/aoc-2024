module Day03  where

import Control.Arrow ((&&&))
import Text.Regex.Posix
import Data.Char (isDigit)

run03 :: String -> (Int, Int)
run03 = (runPt1 &&& runPt2)
  where
    runPt1 = parseInp
    runPt2 = parseInp . (`onoff` True)

parseInp :: String -> Int
parseInp inp = sum . map ((\(a, b) -> ((read @Int) a * (read @Int) b)) . (takeWhile isDigit &&& (tail . dropWhile isDigit)) . init . drop 4) $ getMatches inp

reg :: String
reg = "mul\\([0-9]+,[0-p]+\\)"

getMatches :: String -> [String]
getMatches inp = getAllTextMatches (inp =~ reg :: AllTextMatches [] String)

cdo :: String -> Bool
cdo (d:o:po:pc:_) | [d,o,po,pc] == "do()" = True
cdo _ = False

cdont :: String -> Bool
cdont (d:o:n:ap:t:po:pc:_) | [d,o,n,ap,t,po,pc] == "don't()" = True
cdont _ = False

onoff :: String -> Bool -> String
onoff [] _ = []
onoff t b = if cdo t 
  then onoff (drop 4 t) True
  else if cdont t 
    then onoff (drop 7 t) False
    else if b 
      then head t : onoff (tail t) b
      else onoff (tail t) b
