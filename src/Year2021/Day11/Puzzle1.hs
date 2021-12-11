module Year2021.Day11.Puzzle1 where

import AdventLib.Parsing
import AdventLib.Grids
import Control.Arrow ( second )

-- 1) A discharged squid has the value Nothing.
-- 2) Each field accounts for its own number of flashes.
type Field     = (Int, Maybe Int)
type SquidGrid = Grid Field

grid :: Parser SquidGrid
grid = gridOf ((,) 0 . fmap (read) <$> return . return <$> lexeme digit) 10 10

neighborhood :: Point -> [Point]
neighborhood = bounded_neighborhood 10 10

charge :: Field -> Field
charge = second (fmap (+1))

overCharged :: Field -> Bool
overCharged (_, Just n) = n > 9
overCharged _           = False

uncharge :: Field -> Field
uncharge f@(_, Just _) = f
uncharge   (n, _     ) = (n + 1, Just 0)

step :: SquidGrid -> SquidGrid
step = phase3 . phase2 . phase1
  where
    phase1   = updateAll charge (all_points 10 10)
    phase2 g =
      let ps = [\p h -> (get p h, p)] <*> all_points 10 10 <*> [g]
      in  case filter (overCharged . fst) ps of
            ((_, p) : _) -> phase2 $ updateAll charge (neighborhood p)
                                   $ update (second $ const Nothing) p g
            _            -> g
    phase3 = updateAll uncharge (all_points 10 10)

flashes :: SquidGrid -> Int
flashes = sum . map fst . concat

solution :: String -> IO ()
solution input =
  print $
  (flashes . (flip (foldr (const step)) [1..100])) <$>
  grid `from` input
