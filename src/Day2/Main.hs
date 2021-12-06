module Main where

import Common
import Day2


main :: IO ()
main = do
  let start = Position 0 0 0
  input <- readInput "src/Day2/input.txt"
  let Position {..} = calculatePosition start $ parseCommands (lines input)
  let result = distance * depth
  if result == 1963088820 then print "PASS" else print "FAIL"