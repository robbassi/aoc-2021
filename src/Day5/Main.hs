module Main where

import Day5 (countOverlappingPoints, parseLines, straight)

main :: IO ()
main = do
  input <- lines <$> getContents
  let allLines = parseLines input
      straightLines = filter straight allLines
      part1 = countOverlappingPoints straightLines
      part2 = countOverlappingPoints allLines
  print $ "part 1 = " ++ show part1
  print $ "part 2 = " ++ show part2
