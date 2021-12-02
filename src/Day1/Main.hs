module Main where

type SonarSweepReport = [Int]

numIncreases :: SonarSweepReport -> Int
numIncreases [] = 0
numIncreases [_] = 0
numIncreases (x:xs) = snd $ foldl f (x, 0) xs
  where
    f (x1, n) x2 = (x2, if x2 > x1 then succ n else n)

interpolate :: SonarSweepReport -> SonarSweepReport
interpolate report@(_:_:_:_) = interpolate' report
  where
    interpolate' (x1:x2:x3:xs) = (x1+x2+x3) : interpolate' (x2:x3:xs)
    interpolate' xs = []
interpolate _ = error "invariant violated: input too small"

main :: IO ()
main = do
  let parse = fmap read . lines
  input <- parse <$> getContents
  print $ numIncreases $ interpolate input
