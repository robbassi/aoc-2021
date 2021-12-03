module Day2 where

type Course = [Command]

data Command = Forward Int | Down Int | Up Int deriving (Show, Eq)

data Position = Position {distance :: Int, depth :: Int, aim :: Int} deriving Show


parseCommands :: [String] -> Course
parseCommands = map parseCommand . fmap words
  where
    parseCommand ["forward", n] = Forward $ read n
    parseCommand ["down", n] = Down $ read n
    parseCommand ["up", n]= Up $ read n

calculatePosition :: Position -> Course -> Position
calculatePosition = foldl eval
  where
    eval Position {..} (Forward n) = Position (distance + n) (depth + aim * n) aim
    eval Position {..} (Down n) = Position distance depth (aim + n)
    eval Position {..} (Up n) = Position distance depth (aim - n)
