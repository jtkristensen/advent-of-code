module Year2021.Day07.Puzzle1 where

import AdventLib.Parsing

positions :: Parser [Int]
positions =
  (:) <$> number <*> many (symbol "," >> number) >>= \ns -> eof >> return ns

range :: [Int] -> [Int]
range positions = [minimum positions .. maximum positions]

costs :: (Int -> Int -> Int) -> [Int] -> Int -> [Int]
costs cost positions target =
  do p <- positions
     return $ cost p target

cheapest_move :: [Int] -> Int
cheapest_move positions =
  minimum $ map (sum . costs (\a b -> abs (a - b)) positions) $ range positions

solution :: String -> IO ()
solution input =
  print $
  cheapest_move <$>
  positions `from` input
