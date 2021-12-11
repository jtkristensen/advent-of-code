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

update_all :: (a -> a) -> (Grid a -> Grid a)
update_all f g = updates f (all_points (length g) (length $ head g)) g

x_bound :: Grid a -> Int
x_bound = length

y_bound :: Grid a -> Int
y_bound = length . head

neighborhood :: Grid a -> Point -> [Point]
neighborhood g (i, j) =
  [ (n, m)
  | n <- [i - 1, i, i + 1]
  , m <- [j - 1, j, j + 1]
  , not $ n < 0, n < x_bound g
  , not $ m < 0, m < y_bound g
  ]

adjecent :: Grid a -> Point -> [Point]
adjecent g p@(i, j) =
  filter (/=p) $
  filter (\(k, l) -> i == k || j == l) $
  neighborhood g p

all_points :: Int -> Int -> [Point]
all_points n m =
  do i <- [0..n-1]
     j <- [0..m-1]
     return (i, j)

points_of :: Grid a -> [Point]
points_of g = all_points (x_bound g) (y_bound g)
