module Main where

import Day2

main :: IO ()
main = do
  input <- lines <$> getContents
  let Position {..} = eval defaultPosition $ parseCommands input
  print $ horizontal * depth
