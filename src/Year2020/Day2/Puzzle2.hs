module Year2020.Day2.Puzzle2 where

import AdventLib.Parsing
import Year2020.Day2.Puzzle1 (entry)

check :: String -> Bool
check s =
  let ((i', j', a), pswd) = either (const undefined) id $ entry `from` s
      (i, j) = (i' - 1, j' - 1)
  in (pswd !! i == a || pswd !! j == a) && pswd !! i /= pswd !! j

solution :: String -> IO ()
solution = print . length . (filter check) . lines
