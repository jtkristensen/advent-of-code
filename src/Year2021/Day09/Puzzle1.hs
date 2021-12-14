module Year2021.Day09.Puzzle1 where

import AdventLib.Parsing
import AdventLib.Grids

type HeatMap = Grid Int

heat_map :: Int -> Int -> Parser HeatMap
heat_map = gridOf (read . return <$> lexeme digit)

low :: HeatMap -> [Point]
low m =
  map fst $
  filter (\(p, ns) -> all (get p m<) ns) $
  map (\(p, ps) -> (p, map (\q -> get q m) ps)) $
  map (\p -> (p, adjecent m p)) $
  points_of m

solution :: String -> IO ()
solution input =
  let text   = lines input
      height = length $ text
      width  = length $ head text
  in  print $ sum . (\m -> map ((+1) . flip get m) $ low m) <$>
      heat_map width height `from` input
