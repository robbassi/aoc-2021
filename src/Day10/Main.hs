module Main where

import Data.Maybe (catMaybes, isNothing)
import Data.Foldable (for_)
import Data.List (sort)

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

data SyntaxErrors = SyntaxErrors
  { parens :: Int
  , brackets :: Int
  , braces :: Int
  , angleBrackets :: Int
  }
  deriving Show

type NavigationSubsystem = [[Delimiter]]
type IncompleteLine = [Delimiter]

zeroErrors :: SyntaxErrors
zeroErrors = SyntaxErrors 0 0 0 0

findCorruptDelimiter :: [Delimiter] -> Maybe DelimiterType
findCorruptDelimiter = fst . foldl f (Nothing, [])
  where
    f answer@(Just _, _) _ = answer
    -- init
    f (Nothing, []) (Open delim) = (Nothing, [delim])
    -- invalid, starts with Close
    f (Nothing, []) (Close delim) = (Just delim, [])
    -- push
    f (Nothing, stack) (Open delim) = (Nothing, delim : stack)
    -- pop
    f (Nothing, stack@(delim : open)) (Close delim')
      | delim /= delim' = (Just delim', stack)
      | otherwise = (Nothing, open)

autocomplete :: IncompleteLine -> [DelimiterType]
autocomplete = foldl f []
  where
    -- init
    f [] (Open delim) = [delim]
    -- push
    f stack (Open delim) = delim : stack
    -- pop
    f (_ : open) (Close _) = open

computeErrorScore :: [DelimiterType] -> Int
computeErrorScore delims =
  let SyntaxErrors {..} = mkSyntaxErrors delims
   in parens * 3 + brackets * 57 + braces * 1197 + angleBrackets * 25137

computeAutocompleteScore :: [DelimiterType] -> Int
computeAutocompleteScore = foldl computeScore 0
  where
    computeScore errorScore d = errorScore * 5 + delimScore d
    delimScore Paren = 1
    delimScore Bracket = 2
    delimScore Brace = 3
    delimScore AngleBracket = 4

mkSyntaxErrors :: [DelimiterType] -> SyntaxErrors
mkSyntaxErrors = foldl collect zeroErrors
  where
    collect SyntaxErrors {..} Paren = SyntaxErrors {parens = succ parens, ..}
    collect SyntaxErrors {..} Bracket = SyntaxErrors {brackets = succ brackets, ..}
    collect SyntaxErrors {..} Brace = SyntaxErrors {braces = succ braces, ..}
    collect SyntaxErrors {..} AngleBracket = SyntaxErrors {angleBrackets = succ angleBrackets, ..}

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
      mCorruptLines = map (\line -> (findCorruptDelimiter line, line)) subsystem
      corruptDelimiters = catMaybes $ fst <$> mCorruptLines
      incompleteLines = map snd $ filter (isNothing . fst) mCorruptLines
      completions = autocomplete <$> incompleteLines
      errorScore = computeErrorScore corruptDelimiters
      autocompleteScores = sort $ computeAutocompleteScore <$> completions
      middleAutocompleteScore = autocompleteScores !! (length autocompleteScores `div` 2)
  for_ corruptDelimiters $ \d ->
    print $ "  corrupt: " ++ show d
  print $ "part = " ++ show errorScore
  for_ completions $ \c ->
    print $ "  completion: " ++ show c
  for_ autocompleteScores $ \score ->
    print $ "  score: " ++ show score
  print $ "part 2 = " ++ show middleAutocompleteScore