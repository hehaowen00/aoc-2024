module Main where

import System.IO

parseLine :: String -> [Int]
parseLine line = map read (words line) :: [Int]

checkLevels :: [Int] -> [Int]
checkLevels level = zipWith (-) level (tail level)

isSafe :: [Int] -> Int
isSafe level =
  do
    let report = checkLevels level
    let xs = map abs report
    if (all (> 0) report || all (< 0) report)
      && (maximum xs <= 3 && maximum xs >= 1)
      then 1
      else 0

partOne :: String -> IO ()
partOne contents = do
  let lns = lines contents
      reports = map parseLine lns
      results = map isSafe reports

  print $ sum results

removeAt :: [Int] -> Int -> [Int]
removeAt lst i = take i lst ++ drop (i + 1) lst

isSafeWithDampener :: [Int] -> Int
isSafeWithDampener level =
  if (isSafe level == 1)
    || any (\i -> isSafe (removeAt level i) == 1) [0 .. length level - 1]
    then 1
    else 0

partTwo :: String -> IO ()
partTwo contents = do
  let lns = lines contents
      reports = map parseLine lns
      results = map isSafeWithDampener reports

  print $ sum results

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle

    partOne contents
    partTwo contents