module Main where

import Control.Monad (foldM_)
import Data.Foldable (for_)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Matrix (Matrix)
import qualified Data.Matrix as MX

type Dot = Bool

data Paper = Paper
  { dots :: Matrix Dot,
    rows :: Int,
    cols :: Int
  }
  deriving (Show)

data FoldInstruction
  = Horizontal Int
  | Vertical Int
  deriving (Show)

splitHorizontal :: Paper -> Int -> (Paper, Paper)
splitHorizontal Paper {..} col =
  let (left, right, _, _) = MX.splitBlocks rows col dots
   in ( Paper {dots = left, cols = col, ..},
        Paper {dots = right, cols = cols - col, ..}
      )

splitVertical :: Paper -> Int -> (Paper, Paper)
splitVertical Paper {..} row =
  let (top, _, bottom, _) = MX.splitBlocks row cols dots
   in ( Paper {dots = top, rows = row, ..},
        Paper {dots = bottom, rows = rows - row, ..}
      )

flipHorizontal :: Paper -> Paper
flipHorizontal Paper {..} = Paper {dots = dots', ..}
  where
    dots' = MX.fromLists $ reverse <$> MX.toLists dots

flipVertical :: Paper -> Paper
flipVertical Paper {..} = Paper {dots = dots', ..}
  where
    dots' = MX.fromLists $ reverse $ MX.toLists dots

fold :: Paper -> FoldInstruction -> Paper
fold paper (Horizontal col) =
  let (left, right) = splitHorizontal paper col
      right' = flipHorizontal right
      dots' = MX.elementwise (||) (dots left) (dots right')
   in Paper {dots = dots', cols = col, rows = rows paper}
fold paper (Vertical row) =
  let (top, bottom) = splitVertical paper row
      bottom' = flipVertical bottom
      dots' = MX.elementwise (||) (dots top) (dots bottom')
   in Paper {dots = dots', rows = row, cols = cols paper}

visibleDots :: Paper -> Int
visibleDots = length . filter id . MX.toList . dots

parsePaper :: String -> (Paper, [FoldInstruction])
parsePaper str = (Paper {..}, folds)
  where
    rows = MX.nrows dots
    cols = MX.ncols dots
    dots = parseDots dotPositions
    folds = parseFolds foldInstructions
    [dotPositions, foldInstructions] = splitOn [mempty] $ lines str
    parseDots strs =
      let coords = parseCoord <$> strs
          maxAxis (x, y) (x', y') = (max x x', max y y')
          (maxX, maxY) = foldl maxAxis (0, 0) coords
          dotMap = foldl (\m c -> M.insert c True m) M.empty coords
          dotMatrix = MX.matrix maxY maxX $ \(y, x) ->
            case M.lookup (x, y) dotMap of
              Just _ -> True
              Nothing -> False
       in dotMatrix
    parseCoord str =
      let [x, y] = splitOn "," str
       in (succ $ read x, succ $ read y)
    parseFolds = fmap parseFold
    parseFold str =
      let [_, _, axisAndPosition] = words str
       in case splitOn "=" axisAndPosition of
            ["x", n] -> Horizontal $ read n
            ["y", n] -> Vertical $ read n

prettyDots :: Paper -> IO ()
prettyDots Paper {..} =
  let maxX = MX.ncols dots
      maxY = MX.nrows dots
   in for_ [1 .. maxY] $ \y -> do
        for_ [1 .. maxX] $ \x ->
          if MX.getElem y x dots then putStr "#" else putStr " "
        putStrLn mempty

main :: IO ()
main = do
  input <- getContents
  let (paper, folds@(firstFold : _)) = parsePaper input
      finalPaper = foldl fold paper folds
  print $ "part 1 = " ++ show (visibleDots $ fold paper firstFold)
  print "part 2 = " >> prettyDots finalPaper
