module Main where

import Day4 (computeScore, computeWinners, parseBoards, parseDrawnNumbers)

main :: IO ()
main = do
  (drawnNumberStr : boardStrs) <- lines <$> getContents
  let drawnNumbers = parseDrawnNumbers drawnNumberStr
      boards = parseBoards boardStrs
      winners = computeWinners drawnNumbers boards
      part1 = computeScore $ head winners
      part2 = computeScore $ last winners
  print $ "part 1 = " ++ show part1
  print $ "part 2 = " ++ show part2
