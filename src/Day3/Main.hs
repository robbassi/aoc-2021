{-# LANGUAGE BinaryLiterals #-}

module Main where

import Data.Bits ( Bits((.&.), shift, (.|.), complement) )
import Data.Word ( Word64 )
import Debug.Trace

type Ones = (Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64, Word64)

countOnes :: [String] -> Ones
countOnes = foldl f (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  where
    f (c, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12)
      [b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12] = (
        succ c,
        if b1 == '1' then succ n1 else n1,
        if b2 == '1' then succ n2 else n2,
        if b3 == '1' then succ n3 else n3,
        if b4 == '1' then succ n4 else n4,
        if b5 == '1' then succ n5 else n5,
        if b6 == '1' then succ n6 else n6,
        if b7 == '1' then succ n7 else n7,
        if b8 == '1' then succ n8 else n8,
        if b9 == '1' then succ n9 else n9,
        if b10 == '1' then succ n10 else n10,
        if b11 == '1' then succ n11 else n11,
        if b12 == '1' then succ n12 else n12
      )

computeGamma :: Ones -> Word64
computeGamma (c, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12) =
  selectBit n1 `shiftOne`
  selectBit n2 `shiftOne`
  selectBit n3 `shiftOne`
  selectBit n4 `shiftOne`
  selectBit n5 `shiftOne`
  selectBit n6 `shiftOne`
  selectBit n7 `shiftOne`
  selectBit n8 `shiftOne`
  selectBit n9 `shiftOne`
  selectBit n10 `shiftOne`
  selectBit n11 `shiftOne`
  selectBit n12
  where
    shiftOne a b = shift a 1 .|. b
    len = c `div` 2
    selectBit n = if c - n < len then 1 else 0

data Rating = OxygenGenerator | CO2Scrubber
  deriving (Show, Eq)

bitsToDecimal :: String -> Int
bitsToDecimal str = fromIntegral $ foldl convertStr 0 str
  where
    convertStr :: Word64 -> Char -> Word64
    convertStr n '1' = shift n 1 + 1
    convertStr n '0' = shift n 1

calculateRating :: [String] -> Ones -> Rating -> Int
calculateRating strs' ones rating =
  bitsToDecimal $ head $ fst $ foldl f (strs', ones) [0..11]
  where
    f strs@([answer], ones) index = ([answer], ones)
    f (strs, ones@(c, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12)) index =
      let ans = filter checkBit strs
      in (ans, countOnes ans)
      where
        checkBit str = str !! index == selectBit (ns !! index)
        ns = [n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12]
        selectBit n = case rating of
          OxygenGenerator -> if n >= (c - n) then '1' else '0'
          CO2Scrubber -> if n < (c - n) then '1' else '0'

main :: IO ()
main = do
  input <- lines <$> getContents
  let ones = countOnes input
      gamma = fromIntegral $ computeGamma ones :: Int
      epsilon = fromIntegral $ complement gamma .&. 0b000000000000000000000000000000000000000000000000000111111111111 :: Int
  print $ gamma * epsilon
  let oxygenGeneratorRating = calculateRating input ones OxygenGenerator
      co2ScrubberRating = calculateRating input ones CO2Scrubber
  print oxygenGeneratorRating
  print co2ScrubberRating
  print $ oxygenGeneratorRating * co2ScrubberRating
