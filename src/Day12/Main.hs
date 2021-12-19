module Main where

import Data.Bool (bool)
import Data.Char (isLower)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Cave = String

type Path = [Cave]

type CaveMap = Map Cave [Cave]

findAllPaths :: Int -> CaveMap -> [Path]
findAllPaths maxSmallCaveVisits caveMap =
  explore maxSmallCaveVisits M.empty [] [] "start"
  where
    minCaveVisits = 1
    explore smallCaveLimit visited path paths "end" = reverse ("end" : path) : paths
    explore smallCaveLimit visited path paths curr =
      let visited' =
            if isLower (head curr)
              then M.insertWith (+) curr 1 visited
              else visited
          visits = M.findWithDefault 0 curr visited'
          smallCaveLimit' =
            bool
              maxSmallCaveVisits
              minCaveVisits
              (smallCaveLimit < maxSmallCaveVisits || visits == maxSmallCaveVisits)
          checkVisited "start" = False
          checkVisited cave = (< smallCaveLimit') $ M.findWithDefault 0 cave visited'
          children = M.findWithDefault [] curr caveMap
          unvisited = filter checkVisited children
          paths' = explore smallCaveLimit' visited' (curr : path) paths =<< unvisited
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
