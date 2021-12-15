module Main where

import Data.List (head, last, sortOn)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

{-

STEP  0 | 1 | 2 | 3
====================
CH -> 0   1       2
HH -> 0           1
CB -> 1       2
NH -> 0
HB -> 0   1       3
HC -> 0       1
HN -> 0
NN -> 1
BH -> 0       1   1
NC -> 1   1       1
NB -> 0   1   2   4
BN -> 0           2
BB -> 0       2   4
BC -> 0   1   2   3
CC -> 0       1   1
CN -> 0   1   1   2

Template:     NNCB
After step 1: NCNBCHB
After step 2: NBCCNBBBCBHCB
After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

-}

type InsertionRules = Map (Char, Char) Char

type PairFrequencies = Map (Char, Char) Int

type ElementFrequencies = Map Char Int

data Frequencies = Frequencies
  { pairFrequencies :: PairFrequencies,
    elementFrequencies :: ElementFrequencies
  }
  deriving (Show)

mkFrequencies :: String -> Frequencies
mkFrequencies (t : template) =
  let pairFrequencies = fst $ foldl updateCount (M.empty, t) template
      elementFrequencies = foldl inc M.empty (t : template)
   in Frequencies {..}
  where
    updateCount (m, a) b = (inc m (a, b), b)
    inc m k = M.insertWith (+) k 1 m

simulate :: InsertionRules -> Frequencies -> Int -> Frequencies
simulate rules init steps = foldl run init [1 .. steps]
  where
    run Frequencies {..} _ =
      let updates = transformPair <$> M.toList pairFrequencies
          pairFrequencies' = M.fromListWith (+) $ mconcat $ fst <$> updates
          elementFrequencies' = foldl add elementFrequencies $ snd <$> updates
       in Frequencies pairFrequencies' elementFrequencies'
    transformPair ((a, b), n) =
      let Just c = M.lookup (a, b) rules
       in ([((a, c), n), ((c, b), n)], (c, n))
    add m (k, v) = M.insertWith (+) k v m

computeAnswer :: Frequencies -> Int
computeAnswer Frequencies {..} =
  let sorted = sortOn snd $ M.toList elementFrequencies
      (_, lc) = head sorted
      (_, mc) = last sorted
   in mc - lc

parseInsertionRules :: [String] -> InsertionRules
parseInsertionRules = foldl addRule M.empty
  where
    addRule m s =
      let (k, v) = parseRule s
       in M.insert k v m
    parseRule str =
      let [[a, b], [c]] = splitOn " -> " str
       in ((a, b), c)

main :: IO ()
main = do
  input <- getContents
  let inputLines = lines input
      [[templateStr], insertionRulesStrs] = splitOn [""] inputLines
      insertionRules = parseInsertionRules insertionRulesStrs
      startingFrequencies = mkFrequencies templateStr
      tenSteps = simulate insertionRules startingFrequencies 10
      fourtySteps = simulate insertionRules startingFrequencies 40
  print $ "part 1 = " ++ show (computeAnswer tenSteps)
  print $ "part 2 = " ++ show (computeAnswer fourtySteps)
