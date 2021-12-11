module
  AdventLib.Parsing
  ( Parser
  , module Text.Parsec
  , lexeme
  , number
  , symbol
  , from
  , gridOf
  )
where

import Text.Parsec
import Text.Parsec.String (Parser)

lexeme :: Parser a -> Parser a
lexeme p = p >>= \a -> many space >> return a

number :: Parser Int
number = lexeme $ read <$> many1 digit

symbol :: String -> Parser ()
symbol s = lexeme $ string s >> return ()

from :: Parser a -> String -> Either ParseError a
from p s = parse p "" s

gridOf :: Parser a -> Int -> Int -> Parser [[a]]
gridOf p n m =
    mapM (const $ mapM (const p) [1..n]) [1..m]
