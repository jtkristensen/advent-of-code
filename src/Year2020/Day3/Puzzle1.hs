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
trees sl map = length [ t | t <- path sl map , t == '#' ]

solution :: IO ()
solution =
  trees (3, 1) . lines <$> readFile "./input.txt" >>= print
