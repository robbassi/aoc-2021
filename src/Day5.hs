module Day5 where

import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

type Point = (Int, Int)

data Line = Line Point Point
  deriving (Show)

straight :: Line -> Bool
straight (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

points :: Line -> [Point]
points line@(Line p1@(x1, y1) end@(x2, y2)) = interpolate [p1]
  where
    (rise, run) = (delta y1 y2, delta x1 x2)
    interpolate rest@(p : ps)
      | p == end = rest
      | otherwise =
        let p' = bimap (+ run) (+ rise) p
         in interpolate $ p' : rest
    delta a b =
      case compare a b of
        LT -> 1
        EQ -> 0
        GT -> -1

parseLines :: [String] -> [Line]
parseLines = fmap parseLine
  where
    parseLine str =
      let [p1, p2] = splitOn " -> " str
       in Line (parsePoint p1) (parsePoint p2)
    parsePoint str =
      let [x, y] = read <$> splitOn "," str
       in (x, y)

countOverlappingPoints :: [Line] -> Int
countOverlappingPoints lines = length overlappingPoints
  where
    allPoints = points =<< lines
    countOverlaps m p = M.insertWith (+) p 1 m
    overlaps = foldl countOverlaps M.empty allPoints
    overlappingPoints = filter ((> 1) . snd) $ M.toList overlaps
