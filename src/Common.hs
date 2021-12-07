module Common where

import System.IO
import Control.Monad

import Data.List.Split (splitOn)
import qualified Data.Matrix as M
import qualified Data.Vector as V

readInput :: String -> IO String
readInput fname = openFile fname ReadMode >>= hGetContents

columns :: M.Matrix a -> [V.Vector a]
columns report = [M.getCol i report | i <- [1 .. M.ncols report]]

rows :: M.Matrix a -> [V.Vector a]
rows report = [M.getRow i report | i <- [1 .. M.nrows report]]

parseIntList :: String -> [Int]
parseIntList = fmap read . splitOn ","