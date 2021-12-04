module Year2021.Day2.Puzzle2 where

import Text.Parsec           (parse)
import Year2021.Day2.Puzzle1 (command, Command(..))

data State = State { x, y, aim :: Int } deriving (Show)

run :: Command -> State -> State
run (Forward n) s = s { x   = x s + n
                      , y   = y s + aim s * n }
run (Down    n) s = s { aim = aim s + n }
run (Up      n) s = s { aim = aim s - n }

solution :: [String] -> IO ()
solution = print .
           ((\s -> x s * y s) <$>) .
           foldl (\st cmd -> run <$> parse command "" cmd <*> st)
           (return (State 0 0 0))
