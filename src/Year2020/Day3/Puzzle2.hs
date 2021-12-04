module Year2020.Day3.Puzzle2 where

import Year2020.Day3.Puzzle1 (trees)

solution :: IO ()
solution =
  lines <$> readFile "./input.txt" >>=
  print . product . ([trees] <*> [(1,1), (3,1), (5,1), (7,1), (1,2)] <*>) . return

