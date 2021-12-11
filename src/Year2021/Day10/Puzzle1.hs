module Year2021.Day10.Puzzle1 where

data Status = OK | Corrupted Char | Incomplete [Char]

match :: (Char , Char) -> Bool
match = (`elem` [('(',')'), ('[',']'), ('{','}'), ('<','>')])

isOpen :: Char -> Bool
isOpen = (`elem` ['(', '[', '{', '<'])

status :: String -> Status
status = status' []
  where
    status' [           ] [           ]      = OK
    status' stack@(_ : _) [           ]      = Incomplete stack
    status' stack (c : rest) | isOpen c      = status' (c : stack) rest
    status' [           ] (c : _      )      = Corrupted c
    status' (s:tack) (i:nput) | match (s, i) = status' tack nput
    status' _          input                 = Corrupted $ head input

syntax_error_score :: Status -> Int
syntax_error_score (Corrupted ')') =     3
syntax_error_score (Corrupted ']') =    57
syntax_error_score (Corrupted '}') =  1197
syntax_error_score (Corrupted '>') = 25137
syntax_error_score _               =     0

solution :: String -> IO ()
solution = print . sum . map (syntax_error_score . status) . lines