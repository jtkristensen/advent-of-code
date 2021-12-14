module Year2021.Day03.Puzzle1 where

import Data.List (transpose)

type Bit    = Bool
type Binary = [Bit]

most_common :: Binary -> Bit
most_common bits = length (filter id bits) > length bits `div` 2

to_decimal :: Binary -> Int
to_decimal = sum . map (\(n, b) -> 2^n * b) . zip [0..] . reverse . map fromEnum

gamma, epsilon :: [Binary] -> Binary
gamma   = map most_common
epsilon = map not . gamma

solution :: String -> IO ()
solution input =
  let diagnostic = map (=='1') <$> transpose (lines input)
  in  print $ to_decimal (gamma diagnostic) * to_decimal (epsilon diagnostic)
