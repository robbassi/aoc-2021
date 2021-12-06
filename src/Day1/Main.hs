module Main where
import Common
import Day1

main :: IO ()
main = do
  let parse = fmap read . lines
  let windowSize = 3
  contents <- readInput "src/Day1/input.txt"
  let result = numIncreases $ smooth windowSize (parse contents)
  print $ if result == 1127 then "PASS" else "FAIL"
