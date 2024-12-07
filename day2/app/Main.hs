module Main where

import Control.Monad (when)
import System.IO

parseLine :: String -> [Int]
parseLine line = map read (words line) :: [Int]

isSafe :: [Int] -> Int
isSafe report =
  do
    let xs = map abs report
    if (all (> 0) report || all (< 0) report)
      && (maximum xs <= 3 && maximum xs >= 1)
      then 1
      else 0

partOne :: IO ()
partOne = do
  withFile "input.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle

    let lns = lines contents
        reports = map parseLine lns
        diff = map (\x -> zipWith (-) x (tail x)) reports
        results = map isSafe diff

    print $ sum results

main :: IO ()
main = do
  partOne