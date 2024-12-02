module Main where

import Day01
import Day02

main :: IO ()
main = do 
    readFile "inputs/day01.txt" >>= print . run01
    readFile "inputs/day02.txt" >>= print . run02
