module Year2021.Day10.Puzzle2 where

import Data.List
  (sort)
import Year2021.Day10.Puzzle1
  (Status(..), status)

corrupted :: Status -> Bool
corrupted (Corrupted _) = True
corrupted _             = False

penalty :: Char -> Int
penalty '(' = 1
penalty '[' = 2
penalty '{' = 3
penalty '<' = 4

score :: Status -> Int
score (Incomplete stack) = foldl (\s c -> penalty c + 5 * s) 0 stack
score _                  = 0

middle :: Ord a => [a] -> a
middle as = (sort as) !! (length as `div` 2)

solution :: String -> IO ()
solution = print . middle . filter (/=0) . map (score . status) . lines
