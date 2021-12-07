module Main where

import Common
import Data.Map (Map)
import qualified Data.Map as M

type Days = Int

type Counter = Int

type School = Map Counter Int

birthPeriod :: Int
birthPeriod = 7

daysToMature :: Int
daysToMature = 2

parseSchool :: String -> School
parseSchool = countMap . parseIntList

countMap :: Ord a => [a] -> Map a Int
countMap xs = M.fromListWith (+) [(x, 1) | x <- xs]

simulateDay :: School -> School
simulateDay = M.mapKeysWith (+) decrement . addSpawn
  where
    decrement counter =
      if counter < birthPeriod
        then (counter - 1) `mod` birthPeriod
        else counter - 1
    spawnCount school = M.findWithDefault 0 0 school
    addSpawn school =
      let count = spawnCount school
       in if count == 0 then school else M.insert (daysToMature + birthPeriod) count school

simulate :: Days -> School -> School
simulate 0 school = school
simulate days school = simulate (days - 1) (simulateDay school)

countFish :: School -> Int
countFish = sum . M.elems

main :: IO ()
main = do
  let days = 256
  input <- readInput "src/Day6/input.txt"
  let school = parseSchool input
  let total = countFish $ simulate days school
  print $ if total == 1639643057051 then "PASS" else "FAIL"
