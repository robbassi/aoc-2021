module Main where

import Day5 (countPointsWithMultipleOverlaps, parseLines, straight)

{-
1) parse input
2) foreach line; convert to points; insert points into overlaps map
3) count entries with a value of 2
-}

main :: IO ()
main = do
  input <- lines <$> getContents
  let allLines = parseLines input
      straightLines = filter straight $ parseLines input
      part1 = countPointsWithMultipleOverlaps straightLines
      part2 = countPointsWithMultipleOverlaps allLines
  print $ "part 1 = " ++ show part1
  print $ "part 2 = " ++ show part2
