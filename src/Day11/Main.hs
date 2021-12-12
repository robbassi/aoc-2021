module Main where

import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Data.Foldable (for_)
import Control.Monad (when)

type Point = (Int, Int)
type EnergyLevel = Int
type EnergyLevels = Map Point EnergyLevel

maxX, maxY :: Int
maxX = 9
maxY = 9

get :: EnergyLevels -> Point -> Int
get levels p =
  let Just level = M.lookup p levels
   in level

getNeighbours :: EnergyLevels -> Point -> [Point]
getNeighbours levels (x,y) = filter valid allNeightbours
  where
    allNeightbours =
      -- up, down, left, right
      [(x, pred y), (x, succ y), (pred x, y) ,(succ x, y)
      -- diagonals
      ,(pred x, pred y), (succ x, pred y), (pred x, succ y), (succ x, succ y)]
    valid (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

data OctopusSimulation = OctopusSimulation
  { step :: Int 
  , flashes :: Int
  , energyLevels :: EnergyLevels
  }
  deriving Show

defaultOctopusSimulation :: EnergyLevels -> OctopusSimulation
defaultOctopusSimulation = OctopusSimulation 0 0

simulate :: OctopusSimulation -> Int -> OctopusSimulation
simulate answer 0 = answer
simulate OctopusSimulation {..} days = simulate next $ pred days
  where
    next =
      let updatedLevels = succ' <$> energyLevels
          flashedCells = flashed updatedLevels
          (flashes', energyLevels') = updateAdjacent updatedLevels (S.fromList flashedCells) flashedCells
       in OctopusSimulation { flashes = flashes + flashes', energyLevels = energyLevels', step = succ step }
    succ' n
      | n == 9 = 0
      | otherwise = succ n
    flashed = fmap fst . filter ((== 0) . snd) . M.toList
    updateAdjacent levels flashes [] = (S.size flashes, levels)
    updateAdjacent levels flashes updates = 
      updateAdjacent levels' flashes' updatedAndFlashed'
      where
        filterNotFlashed = filter $ not . (`S.member` flashes)
        neighbours = getNeighbours levels =<< updates
        updates' = filterNotFlashed neighbours
        levels' = foldl update levels updates'
        succ'' n
          | n == 0 = n
          | otherwise = succ' n
        update m p = M.update (Just . succ'') p m
        updatedAndFlashed' = filterNotFlashed $ flashed levels'
        flashes' = foldl (flip S.insert) flashes updatedAndFlashed'

synchronized :: EnergyLevels -> Bool
synchronized = (== 0) . sum

findSynchronizedStep :: OctopusSimulation -> Int 
findSynchronizedStep s@OctopusSimulation {..}
  | synchronized energyLevels = step
  | otherwise = findSynchronizedStep $ simulate s 1

parseEnergyLevels :: String -> EnergyLevels
parseEnergyLevels str = foldl readEnergyLevel M.empty allPoints
  where
    rows = lines str
    allPoints = (,) <$> [0..maxX] <*> [0..maxY]
    readEnergyLevel levels (x,y) =
      let energyLevel = subtract 48 $ ord $ rows !! y !! x
       in M.insert (x,y) energyLevel levels

printGrid :: EnergyLevels -> IO ()
printGrid energyLevels = do
  for_ [0..maxY] $ \y ->
    for_ [0..maxX] $ \x -> do
      putStr $ show $ get energyLevels (x,y)
      when (x == maxX) $ putStrLn ""

main :: IO ()
main = do
  input <- getContents
  let levels = parseEnergyLevels input
      octopusSimulation = defaultOctopusSimulation levels
  print $ ("part 1 = " ++) $ show $ flashes $ simulate octopusSimulation 100
  print $ ("part 2 = " ++) $ show $ findSynchronizedStep octopusSimulation