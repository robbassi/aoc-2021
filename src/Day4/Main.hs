module Main where

import Data.Bool (bool)
import Data.List (find, head, last, partition)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, maybeToList)

maxIndex :: Int
maxIndex = 4

type DrawnNumbers = [Int]

type Pos = (Int, Int)

data Board = Board
  { marked :: Map Pos Bool,
    numMap :: Map Int Pos
  }
  deriving (Show, Eq)

emptyBoard :: Board
emptyBoard =
  Board
    { marked = M.empty,
      numMap = M.empty
    }

parseDrawnNumbers :: String -> DrawnNumbers
parseDrawnNumbers = fmap read . splitOn ","

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards (_ : row1 : row2 : row3 : row4 : row5 : rest) = board : parseBoards rest
  where
    board = emptyBoard {numMap = numMap'}
    numMap' = foldl parseRows M.empty $ zip [0 ..] [row1, row2, row3, row4, row5]
    parseRows numMap (rowNum, numberStrs) = foldl parseCols numMap $ zip [0 ..] $ words numberStrs
      where
        parseCols numMap (colNum, numberStr) =
          let number = read numberStr
           in M.insert number (rowNum, colNum) numMap

checkRow :: Int -> Board -> Bool
checkRow n Board {..} = foldl checkRow' True [0 .. maxIndex]
  where
    checkRow' cond col = M.findWithDefault False (n, col) marked && cond

checkCol :: Int -> Board -> Bool
checkCol n Board {..} = foldl checkCol' True [0 .. maxIndex]
  where
    checkCol' cond row = M.findWithDefault False (row, n) marked && cond

computeWinners :: DrawnNumbers -> [Board] -> [(Board, Int)]
computeWinners drawnNumbers boards = snd $ foldl processNumber (boards, []) drawnNumbers
  where
    processNumber (boards, prevWinners) number = (stillPlaying, prevWinners ++ newWinners')
      where
        boards' = updateBoard <$> boards
        newWinners' = (,number) <$> newWinners
        (newWinners, stillPlaying) = partition isWinner boards'
        updateBoard Board {..} = Board {marked = marked', ..}
          where
            marked' = case M.lookup number numMap of
              Just (row, col) -> M.insert (row, col) True marked
              Nothing -> marked
        isWinner board@Board {numMap} = case M.lookup number numMap of
          Just (row, col) -> checkRow row board || checkCol col board
          Nothing -> False

computeScore :: (Board, Int) -> Int
computeScore (Board {..}, winningNumber) = sum unmarkedNumbers * winningNumber
  where
    unmarkedNumbers = lookupNumber <$> M.toList numMap
    lookupNumber (number, pos) = bool number 0 $ M.findWithDefault False pos marked

main :: IO ()
main = do
  (drawnNumberStr : boardStrs) <- lines <$> getContents
  let drawnNumbers = parseDrawnNumbers drawnNumberStr
      boards = parseBoards boardStrs
      winners = computeWinners drawnNumbers boards
      part1 = computeScore $ head winners
      part2 = computeScore $ last winners
  print $ "part 1 = " ++ show part1
  print $ "part 2 = " ++ show part2
