module Year2021.Day13.Puzzle1 where

import AdventLib.Parsing
import AdventLib.Grids
import Data.List     ( nub   )
import Control.Arrow ( first , second )

data Axis        = X | Y
data Instruction = Fold_Along Axis Int

point :: Parser Point
point = (,) <$> number <*> (symbol "," >> number)

axis :: Parser Axis
axis = choice $ [\(s, t) -> symbol s >> return t] <*> [("x", X), ("y", Y)]

instruction :: Parser Instruction
instruction =
  do symbol "fold"
     symbol "along"
     Fold_Along <$> axis `before` symbol "=" <*> number

transparant :: Parser ([Point], [Instruction])
transparant = (,) <$> many point <*> many instruction `before` eof

transpose :: [Point] -> [Point]
transpose = map $ uncurry $ flip (,)

fold :: Axis -> Int -> [Point] -> [Point]
fold Y n = transpose . fold X n . transpose
fold X n = \points -> nub $ left points ++ right points
  where
    left  = filter ((<n) . fst)
    right = map (first $ \x -> n - (x - n)) . filter ((n<) . fst)

perform :: Instruction -> [Point] -> [Point]
perform (Fold_Along axis n) = fold axis n

solve :: [Point] -> [Instruction] -> [Point]
solve = foldl (flip perform)

solution :: String -> IO ()
solution input =
  do print $
       length . uncurry solve <$> second (return . head) <$>
       transparant `from` input
