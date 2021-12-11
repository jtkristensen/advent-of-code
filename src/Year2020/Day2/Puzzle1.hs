module Year2020.Day2.Puzzle1 where

import AdventLib.Parsing

type Policy   = (Int, Int, Char)
type Password = String

password :: Parser String
password = many1 letter

entry :: Parser (Policy, Password)
entry =
  do min  <- number
     symbol "-"
     max  <- number
     a    <- letter
     symbol ":"
     pswd <- password
     eof
     return ((min, max, a), pswd)

check :: String -> Bool
check s =
  let ((min, max, a), pswd) = either (const undefined) id $ entry `from` s
      n                     = length [ b | b <- pswd , a == b ]
  in min <= n && n <= max

solution :: [String] -> IO ()
solution = print . length . (filter check)
