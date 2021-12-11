module Main where

import Data.Maybe (catMaybes, isNothing)
import Data.Foldable (for_)
import Data.List (sort, subsequences)
import Data.Either (partitionEithers)

data Delimiter
  = Open DelimiterType
  | Close DelimiterType
  deriving Show

data DelimiterType
  = Paren
  | Bracket
  | Brace
  | AngleBracket
  deriving (Show, Eq)

newtype CorruptLine = InvalidClose DelimiterType
  deriving Show

newtype IncompleteLine = IncompleteLine [Delimiter]
  deriving Show

newtype Completion = Completion [DelimiterType]
  deriving Show

data SyntaxErrors = SyntaxErrors
  { parens :: Int
  , brackets :: Int
  , braces :: Int
  , angleBrackets :: Int
  }
  deriving Show

type NavigationSubsystem = [[Delimiter]]

zeroErrors :: SyntaxErrors
zeroErrors = SyntaxErrors 0 0 0 0

findCorruptDelimiter :: [Delimiter] -> Maybe DelimiterType
findCorruptDelimiter = fst . foldl find (Nothing, [])
  where
    find answer@(Just _, _) _ = answer
    -- init
    find (Nothing, []) (Open delim) = (Nothing, [delim])
    -- invalid, starts with Close
    find (Nothing, []) (Close delim) = (Just delim, [])
    -- push
    find (Nothing, stack) (Open delim) = (Nothing, delim : stack)
    -- pop
    find (Nothing, stack@(delim : open)) (Close delim')
      | delim /= delim' = (Just delim', stack)
      | otherwise = (Nothing, open)

analyzeLines :: NavigationSubsystem -> ([CorruptLine], [IncompleteLine])
analyzeLines lines = partitionEithers $ map classify lines
  where
    classify line = case findCorruptDelimiter line of
      Just delim -> Left $ InvalidClose delim
      Nothing -> Right $ IncompleteLine line

autocomplete :: IncompleteLine -> Completion
autocomplete (IncompleteLine delims) = Completion $ foldl closeOpenChunks [] delims
  where
    -- init
    closeOpenChunks [] (Open delim) = [delim]
    -- push
    closeOpenChunks stack (Open delim) = delim : stack
    -- pop
    closeOpenChunks (_ : open) (Close _) = open

computeErrorScore :: [CorruptLine] -> Int
computeErrorScore delims =
  let SyntaxErrors {..} = mkSyntaxErrors delims
   in parens * 3 + brackets * 57 + braces * 1197 + angleBrackets * 25137

computeAutocompleteScore :: Completion -> Int
computeAutocompleteScore (Completion delims) = foldl computeScore 0 delims
  where
    computeScore errorScore d = errorScore * 5 + delimScore d
    delimScore Paren = 1
    delimScore Bracket = 2
    delimScore Brace = 3
    delimScore AngleBracket = 4

mkSyntaxErrors :: [CorruptLine] -> SyntaxErrors
mkSyntaxErrors = foldl increment zeroErrors
  where
    increment SyntaxErrors {..} (InvalidClose Paren) = SyntaxErrors {parens = succ parens, ..}
    increment SyntaxErrors {..} (InvalidClose Bracket) = SyntaxErrors {brackets = succ brackets, ..}
    increment SyntaxErrors {..} (InvalidClose Brace) = SyntaxErrors {braces = succ braces, ..}
    increment SyntaxErrors {..} (InvalidClose AngleBracket) = SyntaxErrors {angleBrackets = succ angleBrackets, ..}

parseNavigationSubsystem :: String -> NavigationSubsystem
parseNavigationSubsystem sourceCode = map parseChunk chunks
  where
    chunks = lines sourceCode
    parseChunk chunk = map parseChunkDelimiter chunk
    parseChunkDelimiter '(' = Open Paren
    parseChunkDelimiter ')' = Close Paren
    parseChunkDelimiter '[' = Open Bracket
    parseChunkDelimiter ']' = Close Bracket
    parseChunkDelimiter '{' = Open Brace
    parseChunkDelimiter '}' = Close Brace
    parseChunkDelimiter '<' = Open AngleBracket
    parseChunkDelimiter '>' = Close AngleBracket

main :: IO ()
main = do
  input <- getContents
  let subsystem = parseNavigationSubsystem input
      (corruptLines, incompleteLines) = analyzeLines subsystem
      completions = autocomplete <$> incompleteLines
      errorScore = computeErrorScore corruptLines
      autocompleteScores = sort $ computeAutocompleteScore <$> completions
      middleAutocompleteScore = autocompleteScores !! (length autocompleteScores `div` 2)
  print $ "part 1 = " ++ show errorScore
  print $ "part 2 = " ++ show middleAutocompleteScore
