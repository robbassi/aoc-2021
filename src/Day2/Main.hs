module Main where

import Day2

main :: IO ()
main = do
  input <- lines <$> getContents
  let Position {..} = eval defaultPosition $ parseCommands input
  print $ "part 1 = " ++ show (horizontal * depth)
  let Position {..} = eval' defaultPosition $ parseCommands input
  print $ "part 2 = " ++ show (horizontal * depth)
