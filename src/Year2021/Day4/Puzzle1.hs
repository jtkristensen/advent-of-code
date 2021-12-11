module Year2021.Day4.Puzzle1 where

import AdventLib.Parsing
import Data.List (transpose)

type Board = [[(Bool, Int)]]
type Game  = ([Int], [Board])

winner :: Board -> Bool
winner board = any (all (id . fst)) (board ++ transpose board)

score :: Board -> Int
score = sum . map snd . filter (not . fst) . concat

brick :: Int -> Board -> Board
brick n = map $ map $ \(b, m) -> (b || m == n, m)

board :: Int -> Int -> Parser Board
board n m =
  mapM (const $ mapM (const $ (,) False <$> number) [1..n]) [1..m]

game :: Int -> Int -> Parser Game
game n m =
  do k  <- number
     ks <- many (symbol "," >> number)
     bs <- many $ board n m
     eof
     return (k : ks, bs)

play :: Game -> Int
play (k : rest, boards) =
  let boards' = map (brick k) boards
  in if   any winner boards'
     then k * (score $ head $ filter winner boards')
     else play (rest, boards')

solution :: String -> IO ()
solution input = print $ play <$> game 5 5 `from` input
