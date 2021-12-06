module Day4 where

import Common
import Data.List (partition)
import Data.List.Split (splitOn)
import Data.Matrix (Matrix, fromLists, mapPos)
import qualified Data.Vector as V

type DrawnNumbers = [Int]

type Board = Matrix (Int, Bool)

type Score = Int

data Strategy = Win | Lose

parseNumbers :: String -> DrawnNumbers
parseNumbers = fmap read . splitOn ","

parseBoards :: [String] -> [Board]
parseBoards = map parseBoard . splitOn [""]
  where
    parseBoard = fromLists <$> map (map buildTuple . words)
    buildTuple = \x -> (read x, False)

playRound :: Int -> [Board] -> [Board]
playRound num = map (mapPos (\(_, _) (x, picked) -> (x, picked || x == num)))

winnersAndLosers :: [Board] -> ([Board],[Board])
winnersAndLosers = partition isWinner
  where
    isWinner board = any ((== True) . checkRowOrCol) $ rows board ++ columns board
    checkRowOrCol = V.all (== True) . V.map snd


calculateScore :: Int -> Board -> Int
calculateScore num board = num * foldl func 0 board
  where
    func score (x, marked) = if marked then score else score + x

playBingo :: DrawnNumbers -> [Board] -> [(Board, Score)]
playBingo numbers boards = playBingo' [] numbers boards
  where
    playBingo' :: [(Board, Score)] -> DrawnNumbers -> [Board] -> [(Board, Score)]
    playBingo' winners [] _ = winners
    playBingo' winners (n : ns) boards =
      let (newWinners, rest) = winnersAndLosers $ playRound n boards
       in playBingo' (winners ++ map (\w -> (w, calculateScore n w)) newWinners) ns rest