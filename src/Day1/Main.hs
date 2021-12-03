module Main where
import System.IO
import Control.Monad

import Day1

main :: IO ()
main = do
  let parse = fmap read . lines
  let windowSize = 3
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ numIncreases $ smooth windowSize (parse contents)

