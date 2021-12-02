module Main where

type SonarSweepReport = [Int]

numIncreases :: SonarSweepReport -> Int
numIncreases [] = 0
numIncreases [_] = 0
numIncreases (x : xs) = snd $ foldl f (x, 0) xs
  where
    f (x1, n) x2
      | x2 > x1 = (x2, succ n)
      | otherwise = (x2, n)

interpolate :: SonarSweepReport -> SonarSweepReport
interpolate (x1 : x2 : x3 : xs) = (x1 + x2 + x3) : interpolate (x2 : x3 : xs)
interpolate _ = []

main :: IO ()
main = do
  let parse = fmap read . lines
  input <- parse <$> getContents
  print $ numIncreases $ interpolate input
