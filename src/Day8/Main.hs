{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List.Split (splitOn)

newtype Pattern = Pattern String
  deriving Show

newtype Digit = Digit String
  deriving Show

data Note = Note
  { patterns :: [Pattern]
  , digits :: [Digit]
  }
  deriving Show

type Notes = [Note]

parseNotes :: [String] -> Notes
parseNotes = fmap parseNote
  where
    parseNote str =
      let [patternStr, digitStr] = splitOn "|" str
          patterns = Pattern <$> words patternStr
          digits = Digit <$> words digitStr
       in Note {..}

main :: IO ()
main = do
 input <- getContents
 let notes = parseNotes $ lines input
 print notes