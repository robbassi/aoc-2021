{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Bits
import Data.List (find, partition)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Word (Word8)

type Segment = Word8

type Segments = Word8

data Note = Note
  { patterns :: [Segments],
    digits :: [Segments]
  }
  deriving (Show)

data KnownDigits = KnownDigits
  { zero :: Segments,
    one :: Segments,
    four :: Segments,
    six :: Segments,
    seven :: Segments,
    eight :: Segments,
    nine :: Segments
  }
  deriving (Show)

data WiringTable = WiringTable
  { wireA :: Segment,
    wireB :: Segment,
    wireC :: Segment,
    wireD :: Segment,
    wireE :: Segment,
    wireF :: Segment,
    wireG :: Segment
  }
  deriving (Show)

segmentA, segmentB, segmentC, segmentD, segmentE, segmentF, segmentG :: Segment
segmentA = bit 0
segmentB = bit 1
segmentC = bit 2
segmentD = bit 3
segmentE = bit 4
segmentF = bit 5
segmentG = bit 6

digitSegments :: Map Segments Int
digitSegments =
  M.fromList
    [ (mkSegments "abcefg", 0),
      (mkSegments "cf", 1),
      (mkSegments "acdeg", 2),
      (mkSegments "acdfg", 3),
      (mkSegments "bcdf", 4),
      (mkSegments "abdfg", 5),
      (mkSegments "abdefg", 6),
      (mkSegments "acf", 7),
      (mkSegments "abcdefg", 8),
      (mkSegments "abcdfg", 9)
    ]

mkSegments :: String -> Segments
mkSegments = foldl (flip lightSegment) 0

lightSegment :: Char -> Segments -> Segments
lightSegment 'a' = (.|. segmentA)
lightSegment 'b' = (.|. segmentB)
lightSegment 'c' = (.|. segmentC)
lightSegment 'd' = (.|. segmentD)
lightSegment 'e' = (.|. segmentE)
lightSegment 'f' = (.|. segmentF)
lightSegment 'g' = (.|. segmentG)

litSegments :: Segments -> Int
litSegments segments = foldl countOnes 0 [0 .. 6]
  where
    countOnes n i
      | bit i .&. segments > 0 = succ n
      | otherwise = n

lookup' :: Ord k => k -> Map k a -> a
lookup' k = fromJust . M.lookup k

findKnownDigits :: [Segments] -> KnownDigits
findKnownDigits patterns =
  let -- these are unique by length
      one = lookup' 2 patternsByLength
      four = lookup' 4 patternsByLength
      seven = lookup' 3 patternsByLength
      eight = lookup' 7 patternsByLength
      -- the only six segment number whose length is 2 off from four is nine
      ([nine], zeroOrSix) = partition ((== 2) . litSegments . xor four) sixSegmentDigits
      -- one is a subset of zero, but not six
      ([zero], [six]) = partition ((== one) . (.&. one)) zeroOrSix
   in KnownDigits {..}
  where
    patternsByLength = M.fromList $ map indexByLength patterns
    indexByLength p = (litSegments p, p)
    sixSegmentDigits = filter ((== 6) . litSegments) patterns

analyzeWiring :: [Segments] -> WiringTable
analyzeWiring patterns =
  let -- the only difference between 1 and 7 is a
      wireA = seven `xor` one
      -- if we remove 1 from 4, and then from d from that, we get b
      wireB = d `xor` (four `xor` one)
      -- c, d and e can be found by removing 6, 0 and 9 from 8 respectively
      wireC = eight `xor` six
      wireD = eight `xor` zero
      wireE = eight `xor` nine
      -- we can find f by removing all the others from 8
      wireF = eight `xor` (wireA .|. wireB .|. wireC .|. wireD .|. wireE .|. wireG)
      -- if we combine 4 and 7, then remove that from 8
      -- we're left with g and e, then we remove e to get g
      wireG = wireE `xor` (eight `xor` (four .|. seven))
   in WiringTable {..}
  where
    KnownDigits {..} = findKnownDigits patterns

decodeNote :: Note -> Int
decodeNote Note {..} = foldl decode 0 digits
  where
    decode n digit = correctDigit digit + n * 10
    correctDigit segments = lookup' rewired digitSegments
      where
        rewired =
          rewire wireA segmentA
            .|. rewire wireB segmentB
            .|. rewire wireC segmentC
            .|. rewire wireD segmentD
            .|. rewire wireE segmentE
            .|. rewire wireF segmentF
            .|. rewire wireG segmentG
        rewire wire segment
          | segments .&. wire > 0 = segment
          | otherwise = 0
        WiringTable {..} = analyzeWiring patterns

countNumbersWithUniqueSegments :: [Segments] -> Int
countNumbersWithUniqueSegments = foldl count 0
  where
    count n segments
      | litSegments segments `elem` [2, 3, 4, 7] = succ n
      | otherwise = n

parseNotes :: [String] -> [Note]
parseNotes = fmap parseNote
  where
    parseNote str =
      let [patternStr, digitStr] = splitOn "|" str
          patterns = mkSegments <$> words patternStr
          digits = mkSegments <$> words digitStr
       in Note {..}

main :: IO ()
main = do
  input <- getContents
  let notes = parseNotes $ lines input
      allDigits = mconcat $ fmap digits notes
      decodedNotes = decodeNote <$> notes
  print $ "part 1 = " ++ show (countNumbersWithUniqueSegments allDigits)
  print $ "part 2 = " ++ show (sum decodedNotes)
