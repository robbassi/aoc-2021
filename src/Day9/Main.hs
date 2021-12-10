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

get :: Heightmap -> Point -> Int
get Heightmap {..} (x,y) = heights !! index
  where
    index = x + y * width

getNeighbours :: Heightmap -> Point -> [Point]
getNeighbours Heightmap {..} (x,y) = filter valid allNeightbours
  where
    allNeightbours = [(x, pred y), (x, succ y), (pred x, y) ,(succ x, y)]
    valid (x, y) = x >= 0 && x < width && y >= 0 && y < height

allPoints :: Heightmap -> [Point]
allPoints Heightmap {..} = (,) <$> [0..pred width] <*> [0..pred height]

parseHeightMap :: String -> Heightmap
parseHeightMap input = Heightmap {..}
  where
    rows = lines input
    nums = mconcat (chunksOf 1 <$> rows)
    height = length rows
    width = length $ head rows
    heights = read <$> nums

lowPoints :: Heightmap -> [Point]
lowPoints hm@Heightmap {..} = foldl checkNeighbours [] $ allPoints hm
  where
    checkNeighbours low point =
      let neighbours = getNeighbours hm point
          neighbourHeights = get hm <$> neighbours
          height = get hm point
          taller = filter (> height) neighbourHeights
       in if length taller == length neighbourHeights
            then point : low
            else low

riskLevel :: Int -> Int
riskLevel = succ

findBasins :: Heightmap -> ([Point], [Basin])
findBasins hm@Heightmap {..} = (points, basins)
  where
    points = lowPoints hm
    basins = findConnected <$> points
    getBasinNeighbours point = filter notTooTall $ getNeighbours hm point
      where
        notTooTall point = get hm point < 9
    findConnected point = go (S.singleton point) (getBasinNeighbours point)
      where
        go ps [] = ps
        go ps next = go ps' unvisitedNeightbours
          where
            ps' = foldl (flip S.insert) ps next
            nextNeightbours = mconcat $ fmap getBasinNeighbours next
            unvisitedNeightbours = filter (not . (`S.member` ps)) nextNeightbours

main :: IO ()
main = do
  input <- getContents
  let heightMap = parseHeightMap input
      (points, basins) = findBasins heightMap
      heights = map (get heightMap) points
      riskLevelsSum = sum $ fmap riskLevel heights
      (top1 : top2 : top3 : _) = reverse $ sort $ fmap S.size basins
      top3Product = top1 * top2 * top3
  print $ "part 1 = " ++ show riskLevelsSum
  -- print basins
  -- print $ fmap S.size basins
  print $ "part 2 = " ++ show top3Product

