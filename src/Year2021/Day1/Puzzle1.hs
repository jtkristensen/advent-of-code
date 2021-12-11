module Year2021.Day1.Puzzle1 where

count :: [Int] -> Int
count xs = length $ filter id $ zipWith (<) xs (tail xs)

solution :: String -> IO ()
solution = print . count . map read . lines
