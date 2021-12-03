module Main where
import System.IO
import Control.Monad

type SonarSweepReport = [Int]

numIncreases :: SonarSweepReport -> Int
numIncreases [] = 0
numIncreases [_] = 0
numIncreases (x : xs) = snd $ foldl f (x, 0) xs
  where
    f (x1, n) x2
      | x2 > x1 = (x2, succ n)
      | otherwise = (x2, n)

smooth :: Int -> SonarSweepReport -> SonarSweepReport
smooth n xs = map sum $ windowed n xs

windowed :: Int -> [a] -> [[a]]
windowed n [] = []
windowed n xs = windowed' (length xs) n xs
  where
    -- Cuts off the tail sub lists that are shorter than the window size
    windowed' cutoff n xs = if cutoff < n then [] else take n xs : windowed' (cutoff - 1) n (tail xs)

main :: IO ()
main = do
  let parse = fmap read . lines
  let windowSize = 3
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ numIncreases $ smooth windowSize (parse contents)

