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
findLowestRisk RiskLevelMap {..} start end = search emptyState
  where
    search state =
      if pos == end
        then totalRisk state end
        else search $ scanFrontier state' $ getFrontier pos
      where
        (pos, state') = minPosition state
        minPosition (risk, prev, unexplored) =
          let (Move pos _, unexplored') = PQ.deleteFindMin unexplored
           in (pos, (risk, prev, unexplored'))
        totalRisk (risk, _, _) pos = lookup risk pos
        scanFrontier state [] = state
        scanFrontier (risk, prev, unexplored) (nextPos : frontier) =
          let posRisk = lookup risk pos
              nextPosRisk = M.findWithDefault maxBound nextPos risk
              nextPosBaseRisk = lookup riskLevels nextPos
              altRisk = posRisk + nextPosBaseRisk
           in if altRisk < nextPosRisk
                then
                  scanFrontier
                    ( -- record the total risk for this position
                      M.insert nextPos altRisk risk,
                      -- record the prev position for the next position
                      M.insert nextPos pos prev,
                      -- update the queue
                      PQ.insert (Move nextPos altRisk) unexplored
                    )
                    frontier
                else scanFrontier (risk, prev, unexplored) frontier
    getFrontier (x, y) = filter valid nextPositions
      where
        nextPositions = [(x, pred y), (x, succ y), (pred x, y), (succ x, y)]
        valid (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY
    emptyState = (M.singleton start 0, M.empty, PQ.singleton (Move start 0))
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
