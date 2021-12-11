module
  AdventLib.Parsing
  ( Parser
  , module Text.Parsec
  , lexeme
  , number
  , symbol
  , from
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
