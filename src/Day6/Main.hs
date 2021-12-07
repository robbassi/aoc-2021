module Main where

import Common
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Days = Int

type Counter = Int

type Amount = Int

type School = Map Counter Amount

parseSchool :: String -> School
parseSchool = countMap . parseIntList

countMap :: Ord a => [a] -> Map a Int
countMap xs = M.fromListWith (+) [(x, 1) | x <- xs]

simulateDay :: School -> School
simulateDay = M.mapKeysWith (+) decrement . addSpawn
  where
    spawnCount school = M.findWithDefault 0 0 school
    addSpawn school = let count = spawnCount school in if count == 0 then school else M.insert 9 count school
    decrement counter = if counter < 7 then (counter - 1) `mod` 7 else counter - 1

simulate :: Days -> School -> School
simulate 0 school = school
simulate days school = simulate (days - 1) (simulateDay school)

countFish :: School -> Int
countFish = sum . M.elems

main :: IO ()
main = do
  input <- readInput "src/Day6/input.txt"
  let school = parseSchool input
  let total = countFish $ simulate 256 school
  print $ if total == 1639643057051 then "PASS" else "FAIL"
