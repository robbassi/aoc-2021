module Main where

import Data.Bits ( Bits((.&.), shift, (.|.), complement) )

data DiagnosticReport = DiagnosticReport
  { total :: Int
  , wordSize :: Int
  , ones :: [Int]
  }
  deriving Show

data Rating = OxygenGenerator | CO2Scrubber
  deriving (Show, Eq)

countOnes :: [String] -> DiagnosticReport
countOnes [] = DiagnosticReport 0 0 mempty
countOnes strs = foldl count (DiagnosticReport 0 wordSize $ replicate wordSize 0) strs
  where
    wordSize = length $ head strs
    count DiagnosticReport {..} str =
      DiagnosticReport  
        { total = succ total
        , ones = zipWith countOne str ones
        , ..
        }
      where
        countOne '1' n = succ n
        countOne _ n = n

computeGamma :: DiagnosticReport -> Int
computeGamma DiagnosticReport {ones=[]} = 0
computeGamma DiagnosticReport {total,ones=(x:xs)} = foldl merge (selectBit x) xs
  where
    merge a b = shift a 1 .|. selectBit b
    selectBit n = if n > (total - n) then 1 else 0

bitsToDecimal :: String -> Int
bitsToDecimal = foldl convertChr 0
  where
    convertChr n '1' = shift n 1 + 1
    convertChr n '0' = shift n 1

calculateRating :: [String] -> DiagnosticReport -> Rating -> Int
calculateRating strs' counts rating =
  bitsToDecimal $ head $ fst $ foldl calculate (strs', counts) [0..wordSize counts]
  where
    calculate acc@([answer], _) _ = acc
    calculate (strs, DiagnosticReport {total,ones}) index = (filtered, countOnes filtered)
      where
        filtered = filter checkBit strs
        checkBit str = str !! index == selectBit (ones !! index)
        selectBit n = case rating of
          OxygenGenerator | n >= (total - n) -> '1'
          CO2Scrubber | n < (total - n) -> '1'
          _ -> '0'

mask :: Int -> Int
mask n = go n 0
  where
    go 0 a = a
    go n a = go (pred n) (shift a 1 + 1)

main :: IO ()
main = do
  input <- lines <$> getContents
  let counts@DiagnosticReport {wordSize} = countOnes input
      gamma = computeGamma counts
      epsilon = complement gamma .&. mask wordSize
  print $ "part 1: " ++ show (gamma * epsilon)

  let oxygenGeneratorRating = calculateRating input counts OxygenGenerator
      co2ScrubberRating = calculateRating input counts CO2Scrubber
  print $ "part 2: " ++ show (oxygenGeneratorRating * co2ScrubberRating)
