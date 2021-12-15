module Year2021.Day14.Puzzle1 where

import AdventLib.Parsing
import AdventLib.Grids
import Data.List     ( sort , groupBy )
import Control.Arrow ( first          )

type Pair      = (Char, Char)
type Polymer   = [(Pair, Integer)]
type Rule      = (Pair, Char)
type Puzzle    = (Polymer, [Rule])
type Matching  = ([Rule], (Pair, Integer))

polymer :: Parser Polymer
polymer =
  do sequence <- lexeme $ many letter
     return $ sequence `zip` (tail sequence) `zip` (repeat 1)

rule :: Parser Rule
rule = (,) <$> (lexeme ((,) <$> letter <*> letter))
           <*> (symbol "->" *> lexeme letter)

puzzle :: Parser Puzzle
puzzle = (,) <$> polymer <*> many rule <* eof

collect :: (Ord a, Ord b, Num b) => [(a, b)] -> [(a, b)]
collect = map (\ps -> (fst $ head ps, sum $ map snd ps)) .
          groupBy (\p q -> fst p == fst q) . sort

partition :: [Rule] -> Polymer -> [Matching]
partition rs ps = [([r | r <- rs , fst r == fst p], p) | p <- ps ]

resolve :: Matching -> Polymer
resolve ([], p          ) = [p]
resolve (rs, ((a, c), n)) =
  concat [[((a, b), n), ((b, c), n)] | (_, b) <- rs]

steps :: Int -> Puzzle -> Puzzle
steps n p = foldl
  (\(q, r) _ -> (collect $ concat $ resolve <$> partition r q, r)) p [1..n]

solve :: Int -> Puzzle -> [(Char, Integer)]
solve n p =
  collect $ ([(snd $ fst $ last (fst p), 1)] ++) $
  map (first fst) $ fst $ steps n p

solution :: String -> IO ()
solution input =
  print $
  (\ns -> maximum ns - minimum ns) .
  map snd .
  solve 10 <$>
  puzzle `from` input

