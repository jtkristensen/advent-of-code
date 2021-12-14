module Year2021.Day02.Puzzle1 where

import AdventLib.Parsing

data Command  = Forward Int | Down Int | Up Int
type Position = (Int, Int)

command :: Parser Command
command =
  choice $ map (\(s, f) -> symbol s >> f <$> number) $
  [ ("forward", Forward) , ("down", Down) , ("up", Up) ]

run :: Command -> Position -> Position
run (Forward n) (x, y) = (x + n, y)
run (Down    n) (x, y) = (x, y + n)
run (Up      n) (x, y) = (x, y - n)

solution :: String -> IO ()
solution = print .
           (uncurry (*) <$>) .
           foldl (\t s -> run <$> command `from` s <*> t)
           (return (0, 0)) .
           lines
