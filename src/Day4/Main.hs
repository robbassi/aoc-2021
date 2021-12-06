module Main where

import Common
import Day4

main :: IO ()
main = do
  (numbersString : _ : boardStrings) <- lines <$> readInput "src/Day4/input.txt"
  let drawnNumbers = parseNumbers numbersString
  let boards = parseBoards boardStrings
  let winners = playBingo drawnNumbers boards
  let winner = head winners
  let loser = last winners
  print $ if snd winner == 4662 && snd loser == 12080 then "PASS" else "FAIL" 
