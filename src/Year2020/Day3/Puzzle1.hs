module Year2020.Day3.Puzzle1 where

every :: Int -> [a] -> [a]
every i = every' i
  where
    every' j (x : rest) | j == i = x : every' 1 rest
    every' j (_ : rest)          = every' (j + 1) rest
    every' _ _                   = []

type Slope = (Int, Int)

path :: Slope -> [String] -> String
path (dx, dy) = zipWith (\i -> (!!(i)) . cycle) [0,dx..] . (every dy)

trees :: Slope -> [String] -> Int
trees sl = length . filter (=='#') . path sl

solution :: [String] -> IO ()
solution = print . trees (3, 1)
