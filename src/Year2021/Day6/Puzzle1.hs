module Year2021.Day6.Puzzle1 where

import AdventLib.Parsing
  ( Parser, listOf, from, number, eof )

type Fish = (Int, Int) -- (how many, inner clock).

int_list :: Parser [Int]
int_list =
  do ns <- listOf number
     eof
     return ns

reduce :: [Fish] -> [Fish]
reduce [             ] = []
reduce ((n, i) : rest) = [(n + (sum $ map fst rest), i)]

collect :: [Fish] -> [Fish]
collect ns =
  concat $ map reduce $
  [filter] <*> ((\n p -> (n==(snd p))) <$> [0..8]) <*> [ns]

step :: Fish -> [Fish]
step (n, 0) = [(n, 6), (n, 8)]
step (n, i) = [(n, i - 1)]

day :: [Fish] -> [Fish]
day ns = collect $ ns >>= step

solution :: String -> IO ()
solution input =
  print $
  sum . map fst .
  flip (foldl (const . day)) [1..80] .
  zip (repeat 1) <$>
  int_list `from` input
