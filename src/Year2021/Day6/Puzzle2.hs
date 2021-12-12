module Year2021.Day6.Puzzle2 where

import AdventLib.Parsing
  ( from )
import Year2021.Day6.Puzzle1
  (int_list, day)

solution :: String -> IO ()
solution input =
  print $
  sum . map fst .
  flip (foldl (const . day)) [1..256] .
  zip (repeat 1) <$>
  int_list `from` input
