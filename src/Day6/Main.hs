module Main where

import Data.List.Split (splitOn)

data School = School
  { zero :: Int,
    one :: Int,
    two :: Int,
    three :: Int,
    four :: Int,
    five :: Int,
    six :: Int,
    seven :: Int,
    eight :: Int
  }
  deriving (Show)

simulateDay School {..} =
  School
    { eight = zero,
      seven = eight,
      six = zero + seven,
      five = six,
      four = five,
      three = four,
      two = three,
      one = two,
      zero = one
    }

simulateDays :: Int -> School -> School
simulateDays 0 school = school
simulateDays n school = simulateDays (pred n) (simulateDay school)

initSchool :: [Int] -> School
initSchool = foldl count (School 0 0 0 0 0 0 0 0 0)
  where
    count s@School {..} n =
      case n of
        0 -> s {zero = succ zero}
        1 -> s {one = succ one}
        2 -> s {two = succ two}
        3 -> s {three = succ three}
        4 -> s {four = succ four}
        5 -> s {five = succ five}
        6 -> s {six = succ six}

schoolSize :: School -> Int
schoolSize School {..} = zero + one + two + three + four + five + six + seven + eight

main :: IO ()
main = do
  [input] <- lines <$> getContents
  let fish = read <$> splitOn "," input
      school = initSchool fish
  print $ "part 1 = " ++ show (schoolSize $ simulateDays 80 school)
  print $ "part 2 = " ++ show (schoolSize $ simulateDays 256 school)
