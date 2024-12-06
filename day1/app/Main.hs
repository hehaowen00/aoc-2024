module Main where

import System.IO
import Data.List

findDistance :: [Int] -> [Int] -> [Int]
findDistance xs ys = zipWith (\x y -> abs (x - y)) xs ys

parseLine :: String -> (Int, Int)
parseLine line = 
    let [left, right] = words line
    in (read left, read right :: Int)

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let lns = lines contents
        results = map words lns
        res = map parseLine lns
        (left, right) = unzip res

    print $ sum $ findDistance (sort left) (sort right) 
