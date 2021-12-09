{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Coerce (coerce)
import Data.List (find, partition)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace

type Segment = Word8
type Segments = Word8

newtype Pattern = Pattern Segments
  deriving Show

newtype Digit = Digit Segments
  deriving Show

data Note = Note
  { patterns :: [Pattern]
  , digits :: [Digit]
  }
  deriving Show

type Notes = [Note]

data Mapping
  = Unknown
  | OneOf Segments
  | Exactly Segment

data KnownSegments =
  KnownSegments
    { one :: Segments
    , four :: Segments
    , seven :: Segments
    , eight :: Segments
    , fiveSegmentDigits :: [Segments]
    , sixSegmentDigits :: [Segments]
    }
  deriving Show

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
  M.fromList [
    (mkSegments "abcefg", 0),
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

showSegments :: Segments -> String
showSegments segments = foldl countOnes "" [0..6]
  where
    countOnes n i
      | bit i .&. segments > 0 = '1' : n
      | otherwise = '0' : n

numSegments :: Segments -> Int
numSegments segments = foldl countOnes 0 [0..6]
  where
    countOnes n i
      | bit i .&. segments > 0 = succ n
      | otherwise = n

findKnownSegments :: [Pattern] -> KnownSegments
findKnownSegments patterns =
  KnownSegments
    { one = findPatternWithLength 2
    , four = findPatternWithLength 4
    , seven = findPatternWithLength 3
    , eight = findPatternWithLength 7
    , fiveSegmentDigits = filter ((== 5) . numSegments) $ fmap coerce patterns
    , sixSegmentDigits = filter ((== 6) . numSegments) $ fmap coerce patterns
    }
  where
    findPatternWithLength n = fromJust $ find ((== n) . numSegments) $ fmap coerce patterns

decodeNote :: Note -> Int
decodeNote Note {..} = foldl calculateDigit 0 digits
  where
    calculateDigit n digit = decodeDigit digit + n * 10
    decodeDigit (Digit segments) = fromJust $ M.lookup decoded digitSegments
      where
        decoded = foldl decodeSegment 0 [0..6]
        decodeSegment decoded i
          | testBit segments i =
            let Just realSegment = M.lookup (bit i) decodedMapping
            in decoded .|. realSegment
          | otherwise = decoded
    decodedMapping =
      M.fromList [
        (a, segmentA),
        (b, segmentB),
        (c, segmentC),
        (d, segmentD),
        (e, segmentE),
        (f, segmentF),
        (g, segmentG)
      ]
    KnownSegments {..} = findKnownSegments patterns
    ([nine], rest) = partition ((== 2) . numSegments . xor four) sixSegmentDigits
    ([zero], [six]) = partition ((== one) . (.&. one)) rest
    a = seven `xor` one
    b = d `xor` bd
    c = eight `xor` six
    d = eight `xor` zero
    e = eight `xor` nine
    f = eight `xor` (a .|. b .|. c .|. d .|. e .|. g)
    g = e `xor` eg
    bd = four `xor` one
    eg = eight `xor` (four .|. seven)

countNumbersWithUniqueSegments :: [Digit] -> Int
countNumbersWithUniqueSegments = foldl count 0
  where
    count n (Digit segments)
      | numSegments segments `elem` [2, 3, 4, 7] = succ n
      | otherwise = n 

parseNotes :: [String] -> Notes
parseNotes = fmap parseNote
  where
    parseNote str =
      let [patternStr, digitStr] = splitOn "|" str
          patterns = Pattern . mkSegments <$> words patternStr
          digits = Digit . mkSegments <$> words digitStr
       in Note {..}

main :: IO ()
main = do
 input <- getContents
 let notes = parseNotes $ lines input
     allDigits = mconcat $ fmap digits notes
     decodedNotes = decodeNote <$> notes
 print $ "part 1 = " ++ show (countNumbersWithUniqueSegments allDigits)
 print $ "part 2 = " ++ show (sum decodedNotes)
