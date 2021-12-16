module Main where

import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (minimumBy)
import Data.Function (on)

type Position = (Int,Int)
type RiskLevels = Map Position Int
type PrevPositions = Map Position Position

data RiskLevelMap = RiskLevelMap
  { riskLevels :: RiskLevels
  , maxX :: Int
  , maxY :: Int
  }
  deriving Show

findShortestPath :: RiskLevelMap -> Position -> Position -> (Int, [Position])
findShortestPath RiskLevelMap {..} start end = (cost, buildPath shortestPath [] (Just end))
  where
    allPositions = (,) <$> [0..maxX] <*> [0..maxY]
    unexplored = S.fromList allPositions
    prevPositions = M.empty
    risk = M.singleton start 0
    buildPath _ path Nothing = path
    buildPath prev path (Just next) = buildPath prev (next:path) (M.lookup next prev)
    (cost, shortestPath) = explore risk prevPositions unexplored
    explore risk prev unexplored =
      if S.null unexplored
        then (M.findWithDefault (error "no answer") end risk, prev)
        else
          let next = minimumBy (compare `on` (\k -> M.findWithDefault maxBound k risk)) unexplored
              allNeightbours = getNeighbours next
              unexploredNeighbours = filter (`S.member` unexplored) allNeightbours
              (risk', prev') = foldl f (risk, prev) unexploredNeighbours
              f (risk, prev) neighbour =
                let Just r = M.lookup next risk
                    neightbourR = M.findWithDefault maxBound neighbour risk
                in case M.lookup neighbour riskLevels of
                    Just neighbourRisk ->
                      let r' = r + neighbourRisk
                      in if r' < neightbourR
                        then (M.insert neighbour r' risk, M.insert neighbour next prev)
                        else (risk, prev)
          in explore risk' prev' (S.delete next unexplored)
    getNeighbours (x, y) = filter valid allNeighbours
      where
        allNeighbours =
          [ (x, pred y),
            (x, succ y),
            (pred x, y),
            (succ x, y)
          ]
        valid (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

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

main :: IO ()
main = do
  input <- getContents
  let riskLevelMap@RiskLevelMap {..} = parseRiskLevelMap input
      (cost, shortestPath) = findShortestPath riskLevelMap (0, 0) (maxX, maxY)
  print riskLevels
  print cost