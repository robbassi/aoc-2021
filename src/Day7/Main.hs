module Main where

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

cheapestPosition :: (Int -> Int) -> [Int] -> Int
cheapestPosition moveCost positions@(p:ps) = foldl findMin (cost 0) possiblePositions
  where
    maxValue = foldl max p ps
    possiblePositions = [0..maxValue]
    findMin currentMin v =
      let diff = cost v
       in if diff < currentMin then diff else currentMin
    cost p = sum $ map (\i -> moveCost $ abs $ i - p) positions

readNums :: String -> [Int]
readNums = fmap read . splitOn ","

main :: IO ()
main = do
  nums <- readNums <$> getContents
  print $ "part 1 = " ++ show (cheapestPosition id nums)
  print $ "part 2 = " ++ show (cheapestPosition crabMoveCost nums)
