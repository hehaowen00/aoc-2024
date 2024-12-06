module Main where

import Control.Monad (when)
import Data.List (lines, sort, sum)
import Data.Map (fromListWith, lookup, toList)
import System.IO
import Prelude hiding (lookup)

findDistance :: [Int] -> [Int] -> [Int]
findDistance = zipWith (\x y -> abs (x - y))

parseLine :: String -> (Int, Int)
parseLine line =
  let [left, right] = words line
   in (read left, read right :: Int)

partOne :: [Int] -> [Int] -> IO ()
partOne leftLst rightLst = do
  print (sum $ findDistance leftLst rightLst)

make :: Int -> (Int, Int)
make val = (val, 1)

similarity :: Maybe Int -> Int -> Int
similarity (Just v) k = k * v
similarity _ _ = 0

partTwo :: [Int] -> [Int] -> IO ()
partTwo leftLst rightLst = do
  let occurences = fromListWith (+) (map make rightLst)
  let scores = map (\x -> similarity (lookup x occurences) x) leftLst
  print $ sum scores

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle

    let lns = lines contents
        res = map parseLine lns
        (left, right) = unzip res

    let leftLst = sort left
    let rightLst = sort right

    partOne leftLst rightLst
    partTwo leftLst rightLst
