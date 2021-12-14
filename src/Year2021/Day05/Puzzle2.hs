module Year2021.Day05.Puzzle2 where

import Data.List
  ( (\\) , nub )

import AdventLib.Parsing
  ( from )

import Control.Arrow
  ( first, second )

import Year2021.Day05.Puzzle1
  ( Point, Segment, segments )

closer :: Int -> Int -> Int
closer target n =
  case target `compare` n of
    LT -> n - 1
    GT -> n + 1
    _  -> n

draw :: Segment -> [Point]
draw (p1, p2) | p1 == p2 = [p1]
draw (p1, p2)            =
  p1 : draw (first (closer (fst p2)) $ second (closer (snd p2)) p1, p2)

solution :: String -> IO ()
solution input =
  print $
  length .
  nub .
  (\ps -> ps \\ nub ps) .
  concat .
  map draw <$>
  segments `from` input
