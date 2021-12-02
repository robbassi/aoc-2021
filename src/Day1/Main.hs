module Main where

type SonarSweepReport = [Int]

numIncreases :: SonarSweepReport -> Int
numIncreases [] = 0
numIncreases [_] = 0
numIncreases (x:xs) = snd $ foldl f (x, 0) xs
 where
   f (x1, n) x2 = (x2, if x2 > x1 then succ n else n)

compressInputs :: SonarSweepReport -> SonarSweepReport
compressInputs report@(x1:x2:x3:xs) = go report
  where
    go (x1:x2:x3:xs) = (x1+x2+x3) : go (x2:x3:xs)
    go xs = []
compressInputs _ = error "invariant violated: input too small"

main :: IO ()
main = do
  let parse :: String -> SonarSweepReport
      parse = fmap read . lines
  input <- parse <$> getContents
  print $ numIncreases $ compressInputs input
