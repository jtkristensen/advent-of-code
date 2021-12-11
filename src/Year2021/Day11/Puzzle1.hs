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
    phase1   = update_all charge
    phase2 g =
      case filter (overCharged . fst) $ [\p h -> (get p h, p)] <*> ps <*> [g] of
        ((_, p) : _) -> phase2 $ updates charge (neighborhood g p)
                               $ update (second $ const Nothing) p g
        _            -> g
    ps     = all_points 10 10
    phase3 = update_all uncharge

flashes :: SquidGrid -> Int
flashes = sum . map fst . concat

solution :: String -> IO ()
solution input =
  print $
  (flashes . (flip (foldr (const step)) [1..100])) <$>
  grid `from` input
