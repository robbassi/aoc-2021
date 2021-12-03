module Common where

import System.IO
import Control.Monad

readInput :: String -> IO String
readInput fname = openFile fname ReadMode >>= hGetContents