module Main where

import Data.Bool (bool)
import Data.List (sort)
import Debug.Trace

type Depth = Int

data Node = Parent Depth Node Node | Leaf Int
  deriving (Show, Eq, Read)

nodeDepth :: Node -> Int
nodeDepth (Parent d _ _) = d
nodeDepth _ = 0

succMaxDepth :: Node -> Node -> Int
succMaxDepth l r = succ $ max (nodeDepth l) (nodeDepth r)

add :: Node -> Node -> Node
add l@(Parent d1 _ _) r@(Parent d2 _ _) =
  let d3 = succ (max d1 d2)
   in reduce $ Parent d3 l r

explode :: Node -> Node
explode root
  | nodeDepth root == 5 = root'
  | otherwise = root
  where
    (root', _, _) = explode' root
    explode' (Parent 2 (Parent 1 (Leaf a) (Leaf b)) r) =
      (Parent (succ $ nodeDepth r) (Leaf 0) (incLeftMost b r), a, 0)
    explode' (Parent 2 (Leaf l) (Parent 1 (Leaf a) (Leaf b))) =
      (Parent 1 (Leaf $ l + a) (Leaf 0), 0, b)
    explode' (Parent d l r)
      | nodeDepth l >= nodeDepth r =
        let (l', ld, rd) = explode' l
         in (Parent (succMaxDepth l' r) l' (incLeftMost rd r), ld, 0)
      | otherwise =
        let (r', ld, rd) = explode' r
         in (Parent (succMaxDepth l r') (incRightMost ld l) r', 0, rd)
    incLeftMost v (Parent d l r) = Parent d (incLeftMost v l) r
    incLeftMost v (Leaf n) = Leaf $ n + v
    incRightMost v (Parent d l r) = Parent d l (incRightMost v r)
    incRightMost v (Leaf n) = Leaf $ n + v

split :: Node -> Node
split = snd . split'
  where
    split' (Parent d l r) =
      let (done, l') = split' l
       in if done
            then (done, Parent (succMaxDepth l' r) l' r)
            else
              let (done, r') = split' r
               in if done
                    then (done, Parent (succMaxDepth l r') l r')
                    else (done, Parent d l r)
    split' (Leaf v)
      | v >= 10 =
        let low = floor $ fromIntegral v / 2
            high = ceiling $ fromIntegral v / 2
         in (True, Parent 1 (Leaf low) (Leaf high))
      | otherwise = (False, Leaf v)

reduce :: Node -> Node
reduce n =
  let n' = split $ reduceExplode n
   in bool (reduce n') n' (n == n')
  where
    reduceExplode n =
      let n' = explode n
       in bool (reduceExplode n') n' (n == n')

magnitude :: Node -> Int
magnitude (Leaf n) = n
magnitude (Parent _ l r) = 3 * magnitude l + 2 * magnitude r

parseTree :: String -> Node
parseTree str = traceDepth tree
  where
    tree = read $ transCode str
    traceDepth n@(Parent _ l r) =
      Parent (treeDepth n 0) (traceDepth l) (traceDepth r)
    traceDepth n = n
    treeDepth (Leaf _) d = d
    treeDepth (Parent _ l r) d =
      max (treeDepth l $ succ d) (treeDepth r $ succ d)

transCode :: String -> String
transCode str = go str ""
  where
    go [] r = r
    go ('[' : s) r = go s (r ++ "(Parent 0 ")
    go (',' : s) r = go s (r ++ " ")
    go (']' : s) r = go s (r ++ ")")
    go (v : s) r = go s (r ++ "(Leaf " ++ [v] ++ ")")

showTree :: Node -> String
showTree (Parent _ l r) = "[" ++ showTree l ++ "," ++ showTree r ++ "]"
showTree (Leaf n) = show n

roundTrip :: String -> Bool
roundTrip s = s == showTree (parseTree s)

main :: IO ()
main = do
  input <- getContents
  let nums@(x : xs) = parseTree <$> lines input
      perms = add <$> nums <*> nums
      highestMagnitude = maximum $ magnitude <$> perms
      total = foldl add x xs
  print $ "part 1 = " ++ show (magnitude total)
  print $ "part 2 = " ++ show highestMagnitude
