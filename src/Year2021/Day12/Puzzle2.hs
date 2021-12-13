module Year2021.Day12.Puzzle2 where

import Year2021.Day12.Puzzle1
  (Cave(..), graph, paths)

import AdventLib.Parsing

solution :: String -> IO ()
solution input =
  print $
  paths Start (1, []) <$>
  graph `from` input
