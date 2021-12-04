module Year2021.Day1.Puzzle2 where

import Year2021.Day1.Puzzle1 (count)

solution :: [String] -> IO ()
solution lines = print $ count $ zipWith3 (\a b c -> a + b + c) l0 l1 l2
  where
    l0 = map read lines
    l1 = tail l0
    l2 = tail l1
