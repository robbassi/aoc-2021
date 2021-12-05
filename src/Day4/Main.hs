{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Main where

import Data.Maybe (fromJust, maybeToList)
import Data.List (find, head, partition)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split (splitOn)

type DrawnNumbers = [Int]
type Pos = (Int, Int)

data Board = Board
  { marked :: Map Pos Bool,
    numMap :: Map Int Pos
  }
  deriving (Show, Eq)

emptyBoard :: Board
emptyBoard = Board
  { marked = M.empty,
    numMap = M.empty
   }

parseDrawnNumbers :: String -> DrawnNumbers
parseDrawnNumbers = fmap read . splitOn ","

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards (_:row1:row2:row3:row4:row5:rest) = board : parseBoards rest
  where
    numMap' = foldl parseRows M.empty $ zip [0..] [row1, row2, row3, row4, row5]
    parseRows numMap (rowNum, numbers) = foldl parseCols numMap $ zip [0..] $ words numbers
      where
        parseCols numMap (colNum, number) = 
          let n = read number
          in M.insert n (rowNum, colNum) numMap
    board = emptyBoard { numMap = numMap' }

checkRow :: Int -> Board -> Bool
checkRow n Board {..} = foldl checkRow' True [0..4]
  where
    checkRow' cond col = M.findWithDefault False (n, col) marked && cond

checkCol :: Int -> Board -> Bool
checkCol n Board {..} = foldl checkCol' True [0..4]
  where
    checkCol' cond row = M.findWithDefault False (row, n) marked && cond

computeWinner :: DrawnNumbers -> [Board] -> (Board, Int)
computeWinner drawnNumbers boards = head $ snd $ foldl processNumber (boards, []) drawnNumbers
  where
    processNumber (boards, prevWinners) number = (stillPlaying, newWinners' ++ prevWinners)
      where
        newWinners' = (,number) <$> newWinners
        (newWinners, stillPlaying) = partition isWinner boards'
        boards' = updateBoard <$> boards
        updateBoard Board {..} = Board {marked = marked', ..}
          where
            marked' = case M.lookup number numMap of
              Just (row, col) -> M.insert (row, col) True marked
              Nothing -> marked
        isWinner board = case M.lookup number $ numMap board of
          Just (row, col) -> checkRow row board || checkCol col board
          Nothing -> False

computeScore :: (Board, Int) -> Int
computeScore (Board {..}, winningNumber) = unmarkedSum * winningNumber
  where
    unmarkedSum = sum (entryToNum <$> M.toList numMap)
    entryToNum (number, pos) = if M.findWithDefault False pos marked
      then 0
      else number

main :: IO ()
main = do
  (drawnNumberStr : boardStrs) <- lines <$> getContents
  let drawnNumbers = parseDrawnNumbers drawnNumberStr
      boards = parseBoards boardStrs
      winner = computeWinner drawnNumbers boards
      score = computeScore winner
  print $ "score = " ++ show score
