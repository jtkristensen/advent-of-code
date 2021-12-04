module Year2021.Day2.Puzzle1 where

import Text.Parsec
import Text.Parsec.String (Parser)

lexeme :: Parser a -> Parser a
lexeme p = p >>= \a -> many space >> return a

number :: Parser Int
number = lexeme $ read <$> many1 digit

symbol :: String -> Parser ()
symbol s = lexeme $ string s >> return ()

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

solution :: [String] -> IO ()
solution = print .
           (uncurry (*) <$>) .
           foldl (\st cmd -> run <$> parse command "" cmd <*> st)
           (return (0, 0))
