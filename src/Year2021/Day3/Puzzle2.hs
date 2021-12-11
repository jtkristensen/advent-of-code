module Year2021.Day3.Puzzle2 where

import Data.List
  ( transpose )
import Year2021.Day3.Puzzle1
  ( Bit
  , Binary
  , to_decimal
  , gamma
  , epsilon
  )

input =
   ["00100"
   ,"11110"
   ,"10110"
   ,"10111"
   ,"10101"
   ,"01111"
   ,"00111"
   ,"11100"
   ,"10000"
   ,"11001"
   ,"00010"
   ,"01010"
   ]

sieve :: (Bit -> Bit) -> Binary -> Int -> [Binary] -> [Binary]
sieve _ _ _ [b]   = [b]
sieve keep b n bs =
  case length (filter p bs) `compare` length (filter (not . p) bs) of
    EQ -> filter (keep . (!!n)) bs
    _  -> filter p bs
  where
    p = ((b !! n) ==) . (!!n)

solve :: (Bit -> Bit) -> Binary -> [Binary] -> Binary
solve p b bs =
  head $ foldl (\e n -> sieve p b n e) bs [0..length b - 1]

solution :: [String] -> IO ()
solution input =
  let diagnostic   = map (=='1') <$> input
      o2_generator = solve id  (gamma   (transpose diagnostic)) diagnostic
      co2_scrubber = solve not (epsilon (transpose diagnostic)) diagnostic
  in  print $ to_decimal o2_generator * to_decimal co2_scrubber
