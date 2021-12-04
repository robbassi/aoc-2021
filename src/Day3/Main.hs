module Main where

import Data.Bits ( Bits((.&.), shift, (.|.), complement) )
import Data.Word ( Word64 )
import Debug.Trace

data Counts = Counts
  { total :: Int
  , wordSize :: Int
  , ones :: [Int]
  }
  deriving Show

countOnes :: [String] -> Counts
countOnes [] = Counts 0 0 mempty
countOnes strs = foldl count (Counts 0 wordSize $ replicate wordSize 0) strs
  where
    wordSize = length $ head strs
    count Counts {..} str = Counts {total = succ total, ones = ones', ..}
      where ones' = zipWith count' str ones
            count' '1' n = succ n
            count' _ n = n

computeGamma :: Counts -> Int
computeGamma Counts {total,ones=(x:xs)} = foldl merge (selectBit x) xs
  where
    merge a b = shift a 1 .|. selectBit b
    selectBit n = if n > (total - n) then 1 else 0

data Rating = OxygenGenerator | CO2Scrubber
  deriving (Show, Eq)

bitsToDecimal :: String -> Int
bitsToDecimal str = fromIntegral $ foldl convertStr 0 str
  where
    convertStr :: Word64 -> Char -> Word64
    convertStr n '1' = shift n 1 + 1
    convertStr n '0' = shift n 1

calculateRating :: [String] -> Counts -> Rating -> Int
calculateRating strs' counts rating =
  bitsToDecimal $ head $ fst $ foldl f (strs', counts) [0..wordSize counts]
  where
    f strs@([answer], ones) index = ([answer], ones)
    f (strs, Counts {total,ones}) index =
      let ans = filter checkBit strs
      in (ans, countOnes ans)
      where
        checkBit str = str !! index == selectBit (ones !! index)
        selectBit n = case rating of
          OxygenGenerator -> if n >= (total - n) then '1' else '0'
          CO2Scrubber -> if n < (total - n) then '1' else '0'

mask :: Int -> Int
mask n = go n 0
  where
    go 0 a = a
    go n a = go (pred n) (shift a 1 + 1)

main :: IO ()
main = do
  input <- lines <$> getContents
  let counts@Counts {wordSize} = countOnes input
      gamma = computeGamma counts
      epsilon = complement gamma .&. mask wordSize
  print $ "part 1: " ++ show (gamma * epsilon)

  let oxygenGeneratorRating = calculateRating input counts OxygenGenerator
      co2ScrubberRating = calculateRating input counts CO2Scrubber
  print $ "part 2: " ++ show (oxygenGeneratorRating * co2ScrubberRating)
