module Year2021.Day1.Puzzle1 where

count :: [Int] -> Int
count [        ] = 0
count (x : rest) = count' x rest
  where
    count' x [        ] = 0
    count' x (y : rest) = (if y > x then 1 else 0) + count' y rest

solution :: [String] -> IO ()
solution = print . count . map read
