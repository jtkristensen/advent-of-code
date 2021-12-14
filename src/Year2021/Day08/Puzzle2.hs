module Year2021.Day08.Puzzle2 where

import AdventLib.Parsing
import Data.List (sort, (\\))

import Year2021.Day08.Puzzle1
  (Entry, alphabet, emap, entries )

type Configuration = Char -> Char

-- TODO : this seems generalizable {^o^}.
configurations :: [Configuration]
configurations =
  map (\f -> \x -> head [y | (z, y) <- f, x == z]) $
  do a <- "abcdefg"
     b <- "abcdefg" \\ [a]
     c <- "abcdefg" \\ [a, b]
     d <- "abcdefg" \\ [a, b, c]
     e <- "abcdefg" \\ [a, b, c, d]
     f <- "abcdefg" \\ [a, b, c, d, e]
     g <- "abcdefg" \\ [a, b, c, d, e, f]
     [[('a', a), ('b', b), ('c', c),
       ('d', d), ('e', e), ('f', f), ('g', g)]]

configure :: Configuration -> Entry String -> Entry String
configure f = emap (sort . map f)

meaningful :: Entry String -> Bool
meaningful (ins, outs) = all (`elem` (map fst alphabet)) $ ins ++ outs

solve :: Entry String -> Entry String
solve e = head $ filter meaningful $ [configure] <*> configurations <*> [e]

read_digit :: String -> Char
read_digit s = head . head $ [show i | (s', i) <- alphabet, s == s']

read_display :: Entry String -> Int
read_display (_, output) = read $ map read_digit output

solution :: String -> IO ()
solution input =
  print $
  sum . map (read_display . solve) <$>
  entries `from` input
