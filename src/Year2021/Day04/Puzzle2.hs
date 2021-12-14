module Year2021.Day04.Puzzle2 where

import AdventLib.Parsing
import Year2021.Day04.Puzzle1
  (Game, winner, score, brick, game, play)

losing :: Game -> Game
losing (k : rest, boards) =
  let boards' = filter (not . winner) $ map (brick k) boards
  in case boards' of
       [_] ->        (rest, boards')
       _   -> losing (rest, boards')

solution :: String -> IO ()
solution input = print $ play . losing <$> game 5 5 `from` input
