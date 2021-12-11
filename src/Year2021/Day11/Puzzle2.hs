module Year2021.Day11.Puzzle2 where

import AdventLib.Parsing
import AdventLib.Grids

import Year2021.Day11.Puzzle1
  (Field, SquidGrid, grid, step, flashes)

steps :: SquidGrid -> Int
steps g =
  let g' = step g
      dx = flashes g' - flashes g
  in 1 + if dx == 100 then 0 else steps g'

solution :: String -> IO ()
solution input =
  print $ steps <$> grid `from` input
