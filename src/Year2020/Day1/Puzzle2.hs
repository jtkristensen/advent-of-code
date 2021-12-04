module Year2020.Day1.Puzzle2 where

solve :: [Int] -> Int
solve ns =
  head $ [ a * b * c | a <- ns , b <- ns , c <- ns , a + b + c == 2020 ]

solution :: IO ()
solution =
  solve . (map read) . lines <$> readFile "./input.txt" >>= print
