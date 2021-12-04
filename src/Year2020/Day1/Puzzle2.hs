module Year2020.Day1.Puzzle2 where

solve :: [Int] -> Int
solve ns =
  head $ [ a * b * c | a <- ns , b <- ns , c <- ns , a + b + c == 2020 ]

solution :: [String] -> IO ()
solution = print . solve . (map read)
