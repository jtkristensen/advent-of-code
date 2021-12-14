module Year2021.Day02.Puzzle2 where

import AdventLib.Parsing
  hiding ( State )

import Year2021.Day02.Puzzle1
  ( command, Command(..) )

data State = State { x, y, aim :: Int } deriving (Show)

run :: Command -> State -> State
run (Forward n) s = s { x   = x s + n
                      , y   = y s + aim s * n }
run (Down    n) s = s { aim = aim s + n }
run (Up      n) s = s { aim = aim s - n }

solution :: String -> IO ()
solution = print .
           ((\s -> x s * y s) <$>) .
           foldl (\t s -> run <$> command `from` s <*> t)
           (return (State 0 0 0)) .
           lines
