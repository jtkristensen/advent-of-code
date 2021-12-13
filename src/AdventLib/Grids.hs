module AdventLib.Grids where

type Grid a = [[a]]
type Point  = (Int, Int)

fresh :: Int -> Int -> a -> Grid a
fresh n m a = take m $ (repeat (take n (repeat a)))

get :: Point -> Grid a -> a
get (i, 0) (row :  _) = row !! i
get (i, j) (_ : rest) = get (i, j - 1) rest

put :: a -> Point -> (Grid a -> Grid a)
put a (i, 0) (row : rest) = (take i row ++ a : drop (i + 1) row) : rest
put a (i, j) (row : rest) = row : put a (i, j - 1) rest

update :: (a -> a) -> Point -> (Grid a -> Grid a)
update f p g = put (f (get p g)) p g

updates :: (a -> a) -> [Point] -> (Grid a -> Grid a)
updates f = foldr (\p g -> g . update f p) id

update_all :: (a -> a) -> (Grid a -> Grid a)
update_all f g = updates f (all_points (length g) (length $ head g)) g

x_bound :: Grid a -> Int
x_bound = length . head

y_bound :: Grid a -> Int
y_bound = length

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
