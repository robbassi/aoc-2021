{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Main where

import Data.List.Split (splitOn)


type Days = Int

type Spawn = (Int, Int)

data School = School
  { zero :: Int
  , one :: Int
  , two :: Int
  , three :: Int
  , four :: Int
  , five :: Int
  , six :: Int
  , seven :: Int
  , eight :: Int
  }
  deriving Show

simulateDay School {..} = School
  { eight = zero
  , seven = eight
  , six = zero + seven
  , five = six
  , four = five
  , three = four
  , two = three
  , one = two
  , zero = one
  }

simulateDays :: Int -> School -> School
simulateDays 0 school = school
simulateDays n school = simulateDays (pred n) (simulateDay school)

initSchool :: [Int] -> School
initSchool = foldl count (School 0 0 0 0 0 0 0 0 0)
  where
    count s n =
      case n of
        0 -> s { zero = zero s + 1 }
        1 -> s { one = one s + 1 }
        2 -> s { two = two s + 1 }
        3 -> s { three = three s + 1 }
        4 -> s { four = four s + 1 }
        5 -> s { five = five s + 1 }
        6 -> s { six = six s + 1 }

main :: IO ()
main = do
  [input] <- lines <$> getContents
  let fish = read <$> splitOn "," input
      School {..} = simulateDays 256 $ initSchool fish
      size = zero + one + two + three + four + five + six + seven + eight
  print $ "part 1 = " ++ show size