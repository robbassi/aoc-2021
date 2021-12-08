module Main where

import Data.Bool (bool)
import Data.List.Split (splitOn)

type Positions = [Int]
type CostFunction = (Int -> Int)
type Difference = Int

crabMoveCost :: Difference -> Int
crabMoveCost n = (n * (n + 1)) `div` 2

leastPossibleFuel :: Positions -> CostFunction -> Int
leastPossibleFuel positions@(p:ps) moveCost = foldl findMin (totalCost 0) possiblePositions
  where
    maxValue = foldl max p ps
    possiblePositions = [1..maxValue]
    findMin currentMin v =
      let diff = totalCost v
       in bool currentMin diff (diff < currentMin)
    totalCost p = sum $ map fuelCost positions
      where
        fuelCost = moveCost . abs . subtract p

readNums :: String -> [Int]
readNums = fmap read . splitOn ","

main :: IO ()
main = do
  nums <- readNums <$> getContents
  let leastPossibleFuelFor = leastPossibleFuel nums
      standardMoveCost = id
  print $ "part 1 = " ++ show (leastPossibleFuelFor standardMoveCost)
  print $ "part 2 = " ++ show (leastPossibleFuelFor crabMoveCost)
