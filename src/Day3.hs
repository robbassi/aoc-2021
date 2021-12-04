module Day3 where

import qualified Data.Matrix as M
import qualified Data.Vector as V

type Report = M.Matrix Int

type ReportRow = V.Vector Int

type ReportCol = V.Vector Int

type BinList = [Int]

parseLine :: String -> BinList
parseLine = map (read . (: ""))

parseReport :: [String] -> Report
parseReport = M.fromLists . map parseLine

columns :: Report -> [ReportCol]
columns report = [M.getCol i report | i <- [1 .. M.ncols report]]

rows :: Report -> [ReportRow]
rows report = [M.getRow i report | i <- [1 .. M.nrows report]]

mostCommonBit :: ReportCol -> Int
mostCommonBit col = if length col - sum col <= sum col then 1 else 0

leastCommonBit :: ReportCol -> Int
leastCommonBit col = if length col - sum col > sum col then 1 else 0

toDecimal :: BinList -> Int
toDecimal list = snd $ foldl func (0, 0) (reverse list)
  where
    func (index, acc) bin =
      if bin == 1
        then (index + 1, acc + 2 ^ index)
        else (index + 1, acc)

computeGamma :: Report -> Int
computeGamma = toDecimal . map mostCommonBit . columns

computeEpsilon :: Report -> Int
computeEpsilon = toDecimal . map leastCommonBit . columns

computePowerConsumption :: Report -> Int
computePowerConsumption report = computeGamma report * computeEpsilon report

-- Prunes rows whose indexed value matches the int
prune :: Int -> Int -> Report -> Report
prune index int report = M.fromLists $ map V.toList $ filter match (rows report)
  where
    match row = row V.! (index - 1) /= int

computeRating :: (ReportCol -> Int) -> Report -> Int
computeRating bitToPrune report = inner 1 report
  where
    inner :: Int -> Report -> Int
    inner index report =
      if M.nrows report == 1
        then toDecimal $ M.toList report
        else inner (index + 1) (prune index (bitToPrune (M.getCol index report)) report)

oxygenGeneratorRating :: Report -> Int
oxygenGeneratorRating = computeRating leastCommonBit

co2ScrubberRating :: Report -> Int
co2ScrubberRating = computeRating mostCommonBit

lifeSupportRating :: Report -> Int
lifeSupportRating report = oxygenGeneratorRating report * co2ScrubberRating report