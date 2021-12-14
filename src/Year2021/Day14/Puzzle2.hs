module Year2021.Day14.Puzzle2 where

import AdventLib.Parsing
  (from)

import Year2021.Day14.Puzzle1
  (solve, puzzle)

solution :: String -> IO ()
solution input =
  print $
  (\ns -> maximum ns - minimum ns) .
  map snd .
  solve 40 <$>
  puzzle `from` input
