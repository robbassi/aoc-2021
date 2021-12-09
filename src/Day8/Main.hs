{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Bits
import Data.List (find, partition)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Debug.Trace

type Segment = Word8

type Segments = Word8

data Note = Note
  { patterns :: [Segments],
    digits :: [Segments]
  }
  deriving (Show)

type Notes = [Note]

data KnownSegments = KnownSegments
  { zero :: Segments,
    one :: Segments,
    four :: Segments,
    six :: Segments,
    seven :: Segments,
    eight :: Segments,
    nine :: Segments,
    sixSegmentDigits :: [Segments]
  }
  deriving (Show)

data TranslationTable = TranslationTable
  { a :: Segments,
    b :: Segments,
    c :: Segments,
    d :: Segments,
    e :: Segments,
    f :: Segments,
    g :: Segments
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
mkSegments = foldl (flip markSegment) 0

markSegment :: Char -> Segments -> Segments
markSegment 'a' = (.|. segmentA)
markSegment 'b' = (.|. segmentB)
markSegment 'c' = (.|. segmentC)
markSegment 'd' = (.|. segmentD)
markSegment 'e' = (.|. segmentE)
markSegment 'f' = (.|. segmentF)
markSegment 'g' = (.|. segmentG)

-- we have an extra bit

numSegments :: Segments -> Int
numSegments segments = foldl countOnes 0 [0 .. 6]
  where
    countOnes n i
      | bit i .&. segments > 0 = succ n
      | otherwise = n

lookup' :: Ord k => k -> Map k a -> a
lookup' k = fromJust . M.lookup k

findKnownSegments :: [Segments] -> KnownSegments
findKnownSegments patterns =
  let one = lookup' 2 patternsByLength
      four = lookup' 4 patternsByLength
      seven = lookup' 3 patternsByLength
      eight = lookup' 7 patternsByLength
      -- the only six segment number whose length is 2 off from four is nine
      ([nine], zeroOrSix) = partition ((== 2) . numSegments . xor four) sixSegmentDigits
      -- zero is a perfect superset of one, six is not
      ([zero], [six]) = partition ((== one) . (.&. one)) zeroOrSix
   in KnownSegments {..}
  where
    patternsByLength = M.fromList $ map indexByLength patterns
    indexByLength p = (numSegments p, p)
    sixSegmentDigits = filter ((== 6) . numSegments) patterns

decodeTranslationTable :: [Segments] -> TranslationTable
decodeTranslationTable patterns =
  let a = seven `xor` one
      b = d `xor` (four `xor` one)
      c = eight `xor` six
      d = eight `xor` zero
      e = eight `xor` nine
      f = eight `xor` (a .|. b .|. c .|. d .|. e .|. g)
      g = e `xor` (eight `xor` (four .|. seven))
   in TranslationTable {..}
  where
    KnownSegments {..} = findKnownSegments patterns

decodeNote :: Note -> Int
decodeNote Note {..} = foldl calculateDigit 0 digits
  where
    calculateDigit n digit = decodeDigit digit + n * 10
    decodeDigit segments = lookup' decoded digitSegments
      where
        decoded =
          decodeSegment a segmentA
            .|. decodeSegment b segmentB
            .|. decodeSegment c segmentC
            .|. decodeSegment d segmentD
            .|. decodeSegment e segmentE
            .|. decodeSegment f segmentF
            .|. decodeSegment g segmentG
        decodeSegment source target
          | segments .&. source > 0 = target
          | otherwise = 0
        TranslationTable {..} = decodeTranslationTable patterns

countNumbersWithUniqueSegments :: [Segments] -> Int
countNumbersWithUniqueSegments = foldl count 0
  where
    count n segments
      | numSegments segments `elem` [2, 3, 4, 7] = succ n
      | otherwise = n

parseNotes :: [String] -> Notes
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
