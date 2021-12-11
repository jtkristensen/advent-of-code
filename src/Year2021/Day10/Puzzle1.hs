module Year2021.Day10.Puzzle1 where

data Status = OK | Corrupted Char | Incomplete [Char]

match :: (Char , Char) -> Bool
match = (`elem` [('(',')'), ('[',']'), ('{','}'), ('<','>')])

isOpen :: Char -> Bool
isOpen = (`elem` ['(', '[', '{', '<'])

status :: String -> Status
status = status' []
  where
    status' [      ] [      ]                = OK
    status' (s:tack) [      ]                = Incomplete (s:tack)
    status' stack    (i:nput) | isOpen i     = status' (i : stack) nput
    status' [      ] (i:_   )                = Corrupted i
    status' (s:tack) (i:nput) | match (s, i) = status' tack nput
    status' _        (i:_   )                = Corrupted i

score :: Status -> Int
score (Corrupted ')') =     3
score (Corrupted ']') =    57
score (Corrupted '}') =  1197
score (Corrupted '>') = 25137
score _               =     0

solution :: String -> IO ()
solution = print . sum . map (score . status) . lines
