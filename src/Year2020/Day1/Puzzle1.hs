module Year2020.Day1.Puzzle1 where

solve :: [Int] -> Int
solve ns =
  head $ [ a * b | a <- ns , b <- ns , a + b == 2020 ]

solution :: String -> IO ()
solution = print . solve . (map read) . lines
