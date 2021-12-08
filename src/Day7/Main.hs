module Main where

import Data.Bool (bool)
import Data.List.Split (splitOn)

crabMoveCost :: Int -> Int
crabMoveCost n = (n * (n + 1)) `div` 2

crabMoveCost' :: Int -> Int 
crabMoveCost' 0 = 0
crabMoveCost' n = n + crabMoveCost' (pred n)

crabMoveCost'' :: Int -> Int 
crabMoveCost'' = (map cost [0..] !!)
  where
    cost 0 = 0
    cost n = n + crabMoveCost'' (pred n)

leastPossibleFuel :: (Int -> Int) -> [Int] -> Int
leastPossibleFuel moveCost positions@(p:ps) = foldl findMin (totalCost 0) possiblePositions
  where
    maxValue = foldl max p ps
    possiblePositions = [1..maxValue]
    findMin currentMin v =
      let diff = totalCost v
       in bool currentMin diff (diff < currentMin)
    totalCost p = sum $ map (fuelCost p) positions
    fuelCost p = moveCost . abs . subtract p

readNums :: String -> [Int]
readNums = fmap read . splitOn ","

main :: IO ()
main = do
  nums <- readNums <$> getContents
  print $ "part 1 = " ++ show (leastPossibleFuel id nums)
  print $ "part 2 = " ++ show (leastPossibleFuel crabMoveCost nums)
