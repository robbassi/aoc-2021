module Main where

import Common
import Day3


main :: IO ()
main = do
  input <- readInput "src/Day3/input.txt"
  let report = parseReport $ lines input
  let firstPart = computePowerConsumption report
  print $ if firstPart == 3813416 then "First Part Passed" else "First Part Failed"
  -- print $ if firstPart == 198 then "First Part Passed" else "First Part Failed"
  let secondPart = lifeSupportRating report
  -- if secondPart == 230 then print secondPart else print "Second Part Failed"
  print $ if secondPart == 2990784 then "Second Part Passed" else print "Second Part Failed"