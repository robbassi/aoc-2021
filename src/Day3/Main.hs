module Main where

import Day3

main :: IO ()
main = do
  input <- lines <$> getContents
  let diagnosticReport@DiagnosticReport {wordSize} = buildDiagnosticReport input
      gamma = computeGamma diagnosticReport
      epsilon = computeEpsilon diagnosticReport gamma
      oxygenGeneratorRating = computeRating input diagnosticReport OxygenGenerator
      co2ScrubberRating = computeRating input diagnosticReport CO2Scrubber
  print $ "part 1: " ++ show (gamma * epsilon)
  print $ "part 2: " ++ show (oxygenGeneratorRating * co2ScrubberRating)
