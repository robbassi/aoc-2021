module Main where

import Data.Bool (bool)
import Data.List (sort)

type Depth = Int

data Node = Parent Depth Node Node | Leaf Int
  deriving (Show, Eq, Read)

depth :: Node -> Int
depth (Parent d _ _) = d
depth _ = 0

succMaxDepth :: Node -> Node -> Int
succMaxDepth left right = succ $ max (depth left) (depth right)

add :: Node -> Node -> Node
add left right = reduce $ Parent (succMaxDepth left right) left right

explode :: Node -> Node
explode root
  | depth root == 5 = root'
  | otherwise = root
  where
    (root', _, _) = explode' root
    explode' (Parent 2 (Parent 1 (Leaf a) (Leaf b)) right) =
      (Parent (succ $ depth right) (Leaf 0) (incLeftMost b right), a, 0)
    explode' (Parent 2 (Leaf left) (Parent 1 (Leaf a) (Leaf b))) =
      (Parent 1 (Leaf $ left + a) (Leaf 0), 0, b)
    explode' (Parent d left right)
      | depth left >= depth right =
        let (left', leftDelta, rightDelta) = explode' left
         in (Parent (succMaxDepth left' right) left' (incLeftMost rightDelta right), leftDelta, 0)
      | otherwise =
        let (right', leftDelta, rightDelta) = explode' right
         in (Parent (succMaxDepth left right') (incRightMost leftDelta left) right', 0, rightDelta)
    incLeftMost delta (Parent d left right) = Parent d (incLeftMost delta left) right
    incLeftMost delta (Leaf v) = Leaf (v + delta)
    incRightMost delta (Parent d left right) = Parent d left (incRightMost delta right)
    incRightMost delta (Leaf v) = Leaf (v + delta)

split :: Node -> Node
split = snd . split'
  where
    split' (Parent d left right) =
      let (done, left') = split' left
       in if done
            then (done, Parent (succMaxDepth left' right) left' right)
            else
              let (done, right') = split' right
               in if done
                    then (done, Parent (succMaxDepth left right') left right')
                    else (done, Parent d left right)
    split' (Leaf v)
      | v >= 10 =
        let low = floor $ fromIntegral v / 2
            high = ceiling $ fromIntegral v / 2
         in (True, Parent 1 (Leaf low) (Leaf high))
      | otherwise = (False, Leaf v)

reduce :: Node -> Node
reduce root = bool (reduce root') root' (root == root')
  where
    root' = split $ reduceExplode root
    reduceExplode root =
      let root' = explode root
       in bool (reduceExplode root') root' (root == root')

magnitude :: Node -> Int
magnitude (Leaf root) = root
magnitude (Parent _ left right) = 3 * magnitude left + 2 * magnitude right

parseTree :: String -> Node
parseTree str = traceDepth tree
  where
    tree = read $ transCode str
    traceDepth root@(Parent _ left right) =
      Parent (treeDepth root 0) (traceDepth left) (traceDepth right)
    traceDepth root = root
    treeDepth (Leaf _) d = d
    treeDepth (Parent _ left right) d =
      max (treeDepth left $ succ d) (treeDepth right $ succ d)

transCode :: String -> String
transCode str = go str ""
  where
    go [] res = res
    go ('[' : s) res = go s (res ++ "(Parent 0 ")
    go (',' : s) res = go s (res ++ " ")
    go (']' : s) res = go s (res ++ ")")
    go (v : s) res = go s (res ++ "(Leaf " ++ [v] ++ ")")

showTree :: Node -> String
showTree (Parent _ left right) = "[" ++ showTree left ++ "," ++ showTree right ++ "]"
showTree (Leaf root) = show root

roundTrip :: String -> Bool
roundTrip s = s == showTree (parseTree s)

main :: IO ()
main = do
  input <- getContents
  let nums@(x : xs) = parseTree <$> lines input
      perms = add <$> nums <*> nums
      sumMagnitude = magnitude $ foldl add x xs
      highestMagnitude = maximum $ magnitude <$> perms
  print $ "part 1 = " ++ show sumMagnitude
  print $ "part 2 = " ++ show highestMagnitude
