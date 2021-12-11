module AdventLib.Grids where

type Grid a = [[a]]
type Point  = (Int, Int)

get :: Point -> Grid a -> a
get (0, j) (row :  _) = row !! j
get (i, j) (_ : rest) = get (i - 1, j) rest

put :: a -> Point -> (Grid a -> Grid a)
put a (0, j) (row : rest) = (take j row ++ a : drop (j + 1) row) : rest
put a (i, j) (row : rest) = row : put a (i - 1, j) rest

update :: (a -> a) -> Point -> (Grid a -> Grid a)
update f p g = put (f (get p g)) p g

updates :: (a -> a) -> [Point] -> (Grid a -> Grid a)
updates f = foldr (\p g -> g . update f p) id

updateAll :: (a -> a) -> (Grid a -> Grid a)
updateAll f g = updates f (all_points (length g) (length $ head g)) g

bounded_neighborhood :: Int -> Int -> Point -> [Point]
bounded_neighborhood i_bound j_bound (i, j) =
  [ (n, m)
  | n <- [i - 1, i, i + 1]
  , m <- [j - 1, j, j + 1]
  , not $ n < 0, n < i_bound
  , not $ m < 0, m < j_bound
  ]

all_points :: Int -> Int -> [Point]
all_points n m =
  do i <- [0..n-1]
     j <- [0..m-1]
     return (i, j)

