module Main where

import Control.Monad
import System.IO

import Day2


main :: IO ()
main = do
  let start = Position 0 0 0
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let Position {..} = calculatePosition start $ parseCommands (lines contents)
  print $ distance * depth