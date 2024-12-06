module Main where

import System.IO
import Data.List

-- findDistance (x:xs) (y:ys) = abs (x - y) : findDistance xs ys
-- findDistance _ _ = []

findDistance xs ys = zipWith (\x y -> abs (x - y))

parseLine :: String -> [Int]
parseLine line = do
    let [left, right] = words line
    [read left, read right :: Int]

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let lns = lines contents
        results = map words lns

    let left = map head results
    let right = map last results

    let leftNums = map (read :: String -> Int) left
    let rightNums = map (read :: String -> Int) right

    print (sum (findDistance (sort leftNums) (sort rightNums)))
