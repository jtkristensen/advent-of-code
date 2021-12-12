module AdventLib.List where

import Data.List (sortOn)

nub :: Ord a => [a] -> [a]
nub as =
  map fst $ sortOn snd $
  clean $
  sortOn fst $ zip as [0..]
  where
    clean [            ] = []
    clean (a : b : rest) | fst a == fst b = clean (b : rest)
    clean (a :     rest)                  = a : clean rest

