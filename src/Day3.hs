module Day3 where

import Data.Bits ( Bits((.&.), shift, (.|.), complement) )

data DiagnosticReport = DiagnosticReport
  { total :: Int
  , wordSize :: Int
  , ones :: [Int]
  }
  deriving Show

data Rating = OxygenGenerator | CO2Scrubber
  deriving (Show, Eq)

defaultDiagnosticReport :: Int -> DiagnosticReport
defaultDiagnosticReport wordSize = DiagnosticReport 0 wordSize $ replicate wordSize 0

buildDiagnosticReport :: [String] -> DiagnosticReport
buildDiagnosticReport strs = foldl add (defaultDiagnosticReport wordSize) strs
  where
    wordSize = length $ head strs
    countOne '1' n = succ n
    countOne _ n = n
    add DiagnosticReport {..} str =
      DiagnosticReport  
        { total = succ total
        , ones = zipWith countOne str ones
        , ..
        }

computeGamma :: DiagnosticReport -> Int
computeGamma DiagnosticReport {total,ones=(x:xs)} = foldl shiftAdd (selectBit x) xs
  where
    shiftAdd a b = shift a 1 .|. selectBit b
    selectBit n
      | n > (total - n) = 1
      | otherwise = 0

computeEpsilon :: DiagnosticReport -> Int -> Int 
computeEpsilon DiagnosticReport {wordSize} gamma = complement gamma .&. mask wordSize 0

computeRating :: [String] -> DiagnosticReport -> Rating -> Int
computeRating strs diagnosticReport rating =
  bitsToDecimal $ head $ fst $ foldl compute (strs, diagnosticReport) [0..wordSize diagnosticReport]
  where
    compute answer@([_], _) _ = answer
    compute (strs, DiagnosticReport {total,ones}) index = (filtered, buildDiagnosticReport filtered)
      where
        filtered = filter checkBit strs
        checkBit str = str !! index == selectBit (ones !! index)
        selectBit n = case rating of
          OxygenGenerator | n >= (total - n) -> '1'
          CO2Scrubber | n < (total - n) -> '1'
          _ -> '0'
    bitsToDecimal = foldl convertChr 0
      where
        convertChr n '1' = shift n 1 + 1
        convertChr n '0' = shift n 1

mask :: Int -> Int -> Int
mask 0 a = a
mask n a = mask (pred n) (shift a 1 + 1)
