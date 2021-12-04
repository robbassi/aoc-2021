module Day3 where

import Data.Bits (Bits (bit, complement, shift, testBit, (.&.), (.|.)))

data DiagnosticReport = DiagnosticReport
  { input :: [Int],
    total :: Int,
    wordSize :: Int,
    ones :: [Int]
  }
  deriving (Show)

data Rating = OxygenGenerator | CO2Scrubber
  deriving (Show, Eq)

buildDiagnosticReport :: Int -> [Int] -> DiagnosticReport
buildDiagnosticReport wordSize input = foldl add defaultDiagnosticReport input
  where
    countOnes num i n
      | bit (wordSize - i - 1) .&. num /= 0 = succ n
      | otherwise = n
    add DiagnosticReport {..} num =
      DiagnosticReport
        { total = succ total,
          ones = zipWith (countOnes num) [0 ..] ones,
          ..
        }
    defaultDiagnosticReport =
      DiagnosticReport
        { total = 0,
          ones = replicate wordSize 0,
          ..
        }

computeGamma :: DiagnosticReport -> Int
computeGamma DiagnosticReport {total, ones = (x : xs)} = foldl shiftAdd (selectBit x) xs
  where
    shiftAdd a b = shift a 1 .|. selectBit b
    selectBit n
      | n > (total - n) = 1
      | otherwise = 0

computeEpsilon :: DiagnosticReport -> Int -> Int
computeEpsilon DiagnosticReport {wordSize} gamma = complement gamma .&. mask wordSize 0
  where
    mask 0 a = a
    mask n a = mask (pred n) (shift a 1 + 1)

computeRating :: DiagnosticReport -> Rating -> Int
computeRating diagnosticReport@DiagnosticReport {..} rating = answer
  where
    DiagnosticReport {input = [answer]} = foldl compute diagnosticReport [0 .. wordSize]
    compute answer@DiagnosticReport {input = [_]} _ = answer
    compute DiagnosticReport {..} index = buildDiagnosticReport wordSize filtered
      where
        filtered = filter checkBit input
        offset = wordSize - index - 1
        checkBit num = selectBit (ones !! index) == testBit num offset
        selectBit n = case rating of
          OxygenGenerator | n >= (total - n) -> True
          CO2Scrubber | n < (total - n) -> True
          _ -> False

bitsToDecimal :: String -> Int
bitsToDecimal = foldl convertChr 0
  where
    convertChr n '1' = shift n 1 + 1
    convertChr n '0' = shift n 1
