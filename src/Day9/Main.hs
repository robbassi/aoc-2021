module Main where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as S
import Data.List.Split (chunksOf)

type Point = (Int, Int)

type Basin = Set Point

data Heightmap = Heightmap
  { heights :: [Int]
  , width :: Int
  , height :: Int
  }
  deriving Show

get :: Heightmap -> Int -> Int -> Int
get Heightmap {..} x y = heights !! index
  where
    index = x + y * width

getNeighbours :: Heightmap -> Int -> Int -> [Point]
getNeighbours Heightmap {..} x y = filter valid allNeightbours
  where
    allNeightbours = [(x, pred y), (x, succ y), (pred x, y) ,(succ x, y)]
    valid (x, y) = x >= 0 && x < width && y >= 0 && y < height

parseHeightMap :: String -> Heightmap
parseHeightMap input = Heightmap {..}
  where
    rows = lines input
    nums = mconcat (chunksOf 1 <$> rows)
    height = length rows
    width = length $ head rows
    heights = read <$> nums

lowPoints :: Heightmap -> [Point]
lowPoints hm@Heightmap {..} =
  foldl scanRow [] [0..pred width]
  where
    scanRow points x = foldl (flip $ checkNeighbours x) points [0..pred height]
    checkNeighbours x y points =
      let neighbours = getNeighbours hm x y
          neighbourHeights = uncurry (get hm) <$> neighbours
          height = get hm x y
          taller = filter (> height) neighbourHeights
       in if length taller == length neighbourHeights
            then (x,y) : points
            else points

riskLevel :: Int -> Int
riskLevel = succ

findBasins :: Heightmap -> ([Point], [Basin])
findBasins hm@Heightmap {..} = (points, basins)
  where
    points = lowPoints hm
    basins = findConnected <$> points
    getBasinNeighbours x y = filter ((/= 9) . uncurry (get hm)) $ getNeighbours hm x y
    findConnected (x,y) = go (S.singleton (x,y)) (getBasinNeighbours x y)
      where
        go :: Set Point -> [Point] -> Basin
        go ps [] = ps
        go ps next = go ps' unvisitedNeightbours
          where
            ps' = foldl (flip S.insert) ps next
            nextNeightbours = mconcat $ fmap (uncurry getBasinNeighbours) next
            unvisitedNeightbours = filter (not . (`S.member` ps)) nextNeightbours

main :: IO ()
main = do
  input <- getContents
  let heightMap = parseHeightMap input
      (points, basins) = findBasins heightMap
      getHeight = uncurry $ get heightMap
      heights = map getHeight points
      riskLevelsSum = sum $ fmap riskLevel heights
      (top1 : top2 : top3 : _) = reverse $ sort $ fmap S.size basins
      top3Product = top1 * top2 * top3
  print $ "part 1 = " ++ show riskLevelsSum
  -- print basins
  -- print $ fmap S.size basins
  print $ "part 2 = " ++ show top3Product

