module Main where

import Data.Bool (bool)
import Data.Char (isLower)
import Data.Foldable (for_)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace

type Cave = String

type Path = [Cave]

type CaveMap = Map Cave [Cave]

findAllPaths :: Int -> CaveMap -> [Path]
findAllPaths maxSmallCaveVisits caveMap =
  explore maxSmallCaveVisits M.empty [] [] "start"
  where
    minCaveVisits = 1
    explore maxSmallCaves visited path paths "end" = reverse ("end" : path) : paths
    explore maxSmallCaves visited path paths curr =
      let children = M.findWithDefault [] curr caveMap
          visited' =
            if isLower (head curr)
              then M.insertWith (+) curr 1 visited
              else visited
          visits = M.findWithDefault 0 curr visited'
          maxSmallCaves' =
            bool
              maxSmallCaveVisits
              minCaveVisits
              ( maxSmallCaves < maxSmallCaveVisits
                  || maxSmallCaves == maxSmallCaveVisits && visits == maxSmallCaveVisits
              )
          checkVisited "start" = False
          checkVisited cave = (< maxSmallCaves') $ M.findWithDefault 0 cave visited'
          unvisited = filter checkVisited children
          paths' = explore maxSmallCaves' visited' (curr : path) paths =<< unvisited
       in paths ++ paths'

parseCaveMap :: String -> CaveMap
parseCaveMap str = M.fromListWith (++) edges
  where
    edges = parseEdges =<< lines str
    parseEdges str =
      let [a, b] = splitOn "-" str
       in [(a, [b]), (b, [a])]

main :: IO ()
main = do
  input <- getContents
  let caveMap = parseCaveMap input
      allPaths = findAllPaths 1 caveMap
      allPaths' = findAllPaths 2 caveMap
  print $ "part 1 = " ++ show (length allPaths)
  print $ "part 2 = " ++ show (length allPaths')
