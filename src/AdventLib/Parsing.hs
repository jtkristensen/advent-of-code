module
  AdventLib.Parsing
  ( Parser
  , module Text.Parsec
  , lexeme
  , number
  , symbol
  , from
  , gridOf
  , listOf
  , before
  )
where

import Text.Parsec
import Text.Parsec.String (Parser)

lexeme :: Parser a -> Parser a
lexeme p = p `before` many space

number :: Parser Int
number = lexeme $ read <$> many1 digit

symbol :: String -> Parser ()
symbol s = lexeme $ string s >> return ()

from :: Parser a -> String -> Either ParseError a
from p s = parse p "" s

gridOf :: Parser a -> Int -> Int -> Parser [[a]]
gridOf p n m =
    mapM (const $ mapM (const p) [1..n]) [1..m]

listOf :: Parser a -> Parser [a]
listOf p = (:) <$> p <*> (many $ symbol "," >> p)

before :: Parser a -> Parser b -> Parser a
before p q = p >>= \a -> q >> return a
