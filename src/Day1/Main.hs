module Main where

import Day1

main :: IO ()
main = do
  let parse = fmap read . lines
  input <- parse <$> getContents
  print $ mappend "part 1 = " $ show $ numIncreases input
  print $ mappend "part 2 = " $ show $ numIncreases $ interpolate input
