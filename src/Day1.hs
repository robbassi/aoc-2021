module Day1 where

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
    windowed' cutoff n xs
      | cutoff < n = []
      | otherwise = take n xs : windowed' (cutoff - 1) n (tail xs)