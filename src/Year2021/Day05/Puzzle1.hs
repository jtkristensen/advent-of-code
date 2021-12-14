module Year2021.Day05.Puzzle1 where

import AdventLib.Parsing
import Data.List ((\\), nub)

type Point   = (Int, Int)
type Segment = (Point, Point)

segment :: Parser Segment
segment =
  do x1 <- number
     symbol ","
     y1 <- number
     symbol "->"
     x2 <- number
     symbol ","
     y2 <- number
     return ((x1, y1), (x2, y2))

segments :: Parser [Segment]
segments =
  do s <- many segment
     eof
     return s

horizontal :: Segment -> Bool
horizontal (p1, p2) = fst p1 == fst p2

vertical :: Segment -> Bool
vertical (p1, p2) = snd p1 == snd p2

draw :: Segment -> [Point]
draw s@((x1, y1), (x2, y2)) =
  case (horizontal s, vertical s) of
    (True, _) -> [(,)] <*> [x1] <*> [min y1 y2..max y1 y2]
    (_, True) -> [(,)] <*> [min x1 x2.. max x1 x2] <*> [y1]
    _         -> []

solution :: String -> IO ()
solution input =
  print $
  length .
  nub .
  (\ps -> ps \\ nub ps) .
  concat .
  map draw <$>
  segments `from` input
