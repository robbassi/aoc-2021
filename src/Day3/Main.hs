module Main where

import Day3

main :: IO ()
main = do
  input <- lines <$> getContents
  let wordSize = length $ head input
      input' = bitsToDecimal <$> input
      diagnosticReport = buildDiagnosticReport wordSize input'
      gamma = computeGamma diagnosticReport
      epsilon = computeEpsilon diagnosticReport gamma
      oxygenGeneratorRating = computeRating diagnosticReport OxygenGenerator
      co2ScrubberRating = computeRating diagnosticReport CO2Scrubber
  print $ "part 1: " ++ show (gamma * epsilon)
  print $ "part 2: " ++ show (oxygenGeneratorRating * co2ScrubberRating)
