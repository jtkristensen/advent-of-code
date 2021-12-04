module Year2020.Day2.Puzzle1 where

import Text.Parsec
import Text.Parsec.String (Parser)

type Policy   = (Int, Int, Char)
type Password = String

lexeme :: Parser a -> Parser a
lexeme p = p >>= \a -> many space >> return a

number :: Parser Int
number = lexeme $ read <$> many1 digit

symbol :: Char -> Parser ()
symbol c = lexeme $ char c >> return ()

password :: Parser String
password = many1 letter

entry :: Parser (Policy, Password)
entry =
  do min  <- number
     symbol '-'
     max  <- number
     a    <- letter
     symbol ':'
     pswd <- password
     eof
     return ((min, max, a), pswd)

check :: String -> Bool
check s =
  let ((min, max, a), pswd) = either (const undefined) id $ parse entry "" s
      n                     = length [ b | b <- pswd , a == b ]
  in min <= n && n <= max

solution :: IO ()
solution =
  length . (filter check) . lines <$> readFile "./input.txt" >>= print
