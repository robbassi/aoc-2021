{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Main where

import Common
import Data.Map (toList, fromListWith)

type Days = Int
type Counter = Int
type Amount = Int
type School = [(Counter, Amount)]

type Spawn = (Int, Int)

data Generation = Generation {school :: School, children :: [Spawn]} deriving Show

parseSchool :: String -> School
parseSchool = count . parseIntList

count :: Ord a => [a] -> [(a, Int)]
count xs = toList (fromListWith (+) [(x, 1) | x <- xs])

simulateDay :: School -> School
simulateDay = map decrement . addSpawn
  where
    spawnCount school = countFish $ filter ((== 0) . fst) school
    addSpawn school = let count = spawnCount school in if count == 0 then school else (9, count) : school
    decrement (counter, amount) = (if counter < 7 then (counter - 1) `mod` 7 else counter - 1, amount)

simulate :: Int -> School -> School
simulate 0 school = school
simulate days school = simulate (days - 1) (simulateDay school)

countFish :: School -> Int
countFish = sum . map snd

main :: IO ()
main = do
  input <- readInput "src/Day6/input.txt"
  let school = parseSchool input
  print $ countFish $ simulate 256 school
  print $ length $ simulate 256 school
