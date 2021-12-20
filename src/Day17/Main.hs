module Main where

import Debug.Trace
import Data.Bool (bool)

data Bounds = Bounds Point Point
  deriving Show

guass :: Int -> Int
guass n = n * (n +  1) `div` 2

type Point = (Int, Int)
type Velocity = (Int, Int)

landsInTarget :: Bounds -> Velocity -> Bool
landsInTarget (Bounds (x1,y1) (x2,y2)) = go (0,0)
  where
    go (x, y) (dx, dy)
      | x >= min x1 x2 && x <= max x1 x2 &&
        y >= min y1 y2 && y <= max y1 y2 = True
      | x > max x1 x2 || y < min y1 y2 = False
      | otherwise =
          go
            ( x + dx, y + dy )
            ( if dx == 0 then dx else pred dx
            , pred dy )

possibleVelocities :: Bounds -> [Velocity]
possibleVelocities (Bounds (x1,y1) (x2,y2)) = (,) <$> [0 .. max x1 x2] <*> [minY .. maxY]
  where
    minY = negate $ abs $ min y1 y2
    maxY = abs $ min y1 y2

highestY :: Bounds -> Int
highestY b = maximum $ snd <$> filter (landsInTarget b) (possibleVelocities b)

countPaths :: Bounds -> Int
countPaths b = length $ filter (landsInTarget b) (possibleVelocities b)

parseBounds :: String -> Bounds
parseBounds str = undefined

main :: IO ()
main = do
  input <- getContents
  let bounds = Bounds (79,-176) (137,-117)
      paths = countPaths bounds
      maxY = highestY bounds
  print $ "part 1 = " ++ show (guass maxY)
  print $ "part 2 = " ++ show paths
