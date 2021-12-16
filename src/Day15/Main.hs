module Main where

import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ

type Position = (Int, Int)

type RiskLevels = Map Position Int

type PrevPositions = Map Position Position

data RiskLevelMap = RiskLevelMap
  { riskLevels :: RiskLevels,
    maxX :: Int,
    maxY :: Int
  }
  deriving (Show)

data Move = Move Position Int
  deriving (Show, Eq)

instance Ord Move where
  compare (Move _ r) (Move _ r') = compare r r'

findLowestRisk :: RiskLevelMap -> Position -> Position -> Int
findLowestRisk RiskLevelMap {..} start end = explore risk prevPositions unexplored
  where
    -- start with the source
    unexplored = PQ.singleton (Move start 0)
    prevPositions = M.empty
    -- we don't take any risk for the start position
    risk = M.singleton start 0
    -- explore the map until we hit the end
    explore risk prev unexplored =
      let -- grab the min position from the queue
          (Move currentPosition _, unexplored') = PQ.deleteFindMin unexplored
          -- get next positions the explore
          nextPositions = getNeighbours currentPosition
          -- explore the map, and return the new state as we find better moves
          searchFrontier (risk, prev, unexplored) nextPosition =
            let currentRisk = lookup risk currentPosition
                nextRiskTotal = M.findWithDefault maxBound nextPosition risk
                nextRiskBase = lookup riskLevels nextPosition
                alternativeRisk = currentRisk + nextRiskBase
             in -- if we find a better move, update the state
                if alternativeRisk < nextRiskTotal
                  then
                    ( -- record the total risk for this position
                      M.insert nextPosition alternativeRisk risk,
                      -- record the prev position for the next position
                      M.insert nextPosition currentPosition prev,
                      -- update the queue
                      PQ.insert (Move nextPosition alternativeRisk) unexplored
                    )
                  else (risk, prev, unexplored)
          -- compute the new state
          (risk', prev', unexplored'') = foldl searchFrontier (risk, prev, unexplored') nextPositions
       in -- check if we found the end
          if currentPosition == end
            then -- return the total risk for the optimal path
              lookup risk' end
            else -- continue exploring
              explore risk' prev' unexplored''
    getNeighbours (x, y) = filter valid nextPositions
      where
        nextPositions = [(x, pred y), (x, succ y), (pred x, y), (succ x, y)]
        valid (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY
    -- lookup that assumes existense
    lookup m = fromJust . flip M.lookup m

parseRiskLevelMap :: String -> RiskLevelMap
parseRiskLevelMap str = RiskLevelMap {..}
  where
    riskLevels = foldl readEnergyLevel M.empty allPoints
    maxX = pred $ length $ head rows
    maxY = pred $ length rows
    rows = lines str
    allPoints = (,) <$> [0 .. maxX] <*> [0 .. maxY]
    readEnergyLevel levels (x, y) =
      let energyLevel = subtract 48 $ ord $ rows !! y !! x
       in M.insert (x, y) energyLevel levels

expandMap :: RiskLevelMap -> RiskLevelMap
expandMap RiskLevelMap {..} = RiskLevelMap riskLevels' maxX' maxY'
  where
    maxX' = pred $ succ maxX * 5
    maxY' = pred $ succ maxY * 5
    riskLevels' = M.fromList $ generatePositions =<< M.toList riskLevels
    generatePositions ((x, y), r) = foldl addScaledPosition [] offsets
      where
        addScaledPosition ps (dx, dy) =
          let x' = x + succ maxX * dx
              y' = y + succ maxY * dy
              r' = wrapRisk (r + dx + dy)
           in ((x', y'), r') : ps
    offsets = (,) <$> [0 .. 4] <*> [0 .. 4]
    wrapRisk n = if n > 9 then succ (n `mod` 10) else n

main :: IO ()
main = do
  input <- getContents
  let smallMap = parseRiskLevelMap input
      costSmall = findLowestRisk smallMap (0, 0) (maxX smallMap, maxY smallMap)
      fullMap = expandMap smallMap
      costFull = findLowestRisk fullMap (0, 0) (maxX fullMap, maxY fullMap)
  print $ "part 1 = " ++ show costSmall
  print $ "part 2 = " ++ show costFull
