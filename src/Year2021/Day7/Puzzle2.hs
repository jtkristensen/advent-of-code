module Year2021.Day7.Puzzle2 where

import AdventLib.Parsing

import Year2021.Day7.Puzzle1
  ( positions , range , costs )

cheapest_move :: [Int] -> Int
cheapest_move positions =
  minimum $ map (sum . costs (\a b -> sum [1..abs (a - b)]) positions) $ range positions

solution :: String -> IO ()
solution input =
  print $
  cheapest_move <$>
  positions `from` input
