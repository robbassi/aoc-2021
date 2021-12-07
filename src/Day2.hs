module Day2 where

type Course = [Command]

data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show, Eq)

data Position = Position
  { horizontal :: Int,
    depth :: Int,
    aim :: Int
  }
  deriving (Show)

defaultPosition :: Position
defaultPosition = Position 0 0 0

parseCommands :: [String] -> Course
parseCommands = map parseCommand . fmap words
  where
    parseCommand ["forward", n] = Forward $ read n
    parseCommand ["down", n] = Down $ read n
    parseCommand ["up", n] = Up $ read n

eval :: Position -> Course -> Position
eval = foldl eval'
  where
    eval' Position {..} (Forward n) = Position (horizontal + n) depth aim
    eval' Position {..} (Down n) = Position horizontal (depth + n) aim
    eval' Position {..} (Up n) = Position horizontal (depth - n) aim

eval' :: Position -> Course -> Position
eval' = foldl eval'
  where
    eval' Position {..} (Forward n) = Position (horizontal + n) (depth + aim * n) aim
    eval' Position {..} (Down n) = Position horizontal depth (aim + n)
    eval' Position {..} (Up n) = Position horizontal depth (aim - n)
