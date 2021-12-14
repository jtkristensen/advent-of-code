module Year2021.Day09.Puzzle2 where

import AdventLib.Parsing
import AdventLib.Grids

import Year2021.Day09.Puzzle1
  (HeatMap, heat_map, low)

import Data.List (sort, nub)

type Basin = [Point]

expand :: HeatMap -> Basin -> Basin
expand h b =
  let extra = filter (\p -> not (p `elem` b) && get p h /= 9) $ b >>= adjecent h
  in  nub $ b ++ extra

basin :: HeatMap -> Point -> Basin
basin h p = basin' [p]
  where
    basin' ps =
      let ps' = expand h ps
      in  if ps == ps'
          then ps
          else basin' ps'

basin_sizes :: HeatMap -> [Int]
basin_sizes h = map length $ nub $ map (sort . basin h) (low h)

solution :: String -> IO ()
solution input =
  let text   = lines input
      height = length $ text
      width  = length $ head text
      hm     = heat_map width height `from` input
  in  print $ product . take 3 . reverse . sort . basin_sizes <$> hm
