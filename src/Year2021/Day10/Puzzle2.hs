module Year2021.Day10.Puzzle2 where

import Data.List
  (sort)
import Year2021.Day10.Puzzle1
  (Status(..), status)

corrupted :: Status -> Bool
corrupted (Corrupted _) = True
corrupted _             = False

autocomplete_penalty :: Char -> Int
autocomplete_penalty '(' = 1
autocomplete_penalty '[' = 2
autocomplete_penalty '{' = 3
autocomplete_penalty '<' = 4

autocomplete_score :: Status -> Int
autocomplete_score (Incomplete stack) =
  foldl (\s c -> autocomplete_penalty c + 5 * s) 0 stack
autocomplete_score _ = 0

middle :: Ord a => [a] -> a
middle as = (sort as) !! (length as `div` 2)

solution :: String -> IO ()
solution = print . middle . filter (/=0) . map (autocomplete_score . status) . lines
