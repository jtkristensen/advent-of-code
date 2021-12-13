module Year2021.Day13.Puzzle1 where

import AdventLib.Parsing
import AdventLib.Grids
import Data.List     ( nub   )
import Control.Arrow ( first , second )

type Axis        = Char
type Instruction = [Point] -> [Point]

point :: Parser Point
point = (,) <$> number <*> (symbol "," >> number)

instruction :: Parser Instruction
instruction =
  do symbol "fold"
     symbol "along"
     fold <$> letter <*> (symbol "=" >> number)

transparant :: Parser ([Point], [Instruction])
transparant = (,) <$> many point <*> many instruction `before` eof

transpose :: [Point] -> [Point]
transpose = map $ uncurry $ flip (,)

fold :: Axis -> Int -> [Point] -> [Point]
fold 'y' n = transpose . fold 'x' n . transpose
fold _   n = \points -> nub $ left points ++ right points
  where
    left  = filter ((<n) . fst)
    right = map (first $ \x -> n - (x - n)) . filter ((n<) . fst)

solve :: [Point] -> [Instruction] -> [Point]
solve = foldl (flip ($))

solution :: String -> IO ()
solution input =
  print $
  length . uncurry solve <$> second (return . head) <$>
  transparant `from` input
