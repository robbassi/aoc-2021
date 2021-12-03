module Main where

import Day1

main :: IO ()
main = do
  let parse = fmap read . lines
  input <- parse <$> getContents
  print $ numIncreases $ interpolate input
