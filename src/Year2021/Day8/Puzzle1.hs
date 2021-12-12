module Year2021.Day8.Puzzle1 where

import AdventLib.Parsing
import Data.List (nub, sort)

type Entry a = ([a], [a])

alphabet :: [(String, Int)]
alphabet = [("abcefg", 0), ("cf", 1), ("acdeg", 2),
            ("acdfg",  3), ("bcdf", 4), ("abdfg", 5),
            ("abdefg", 6), ("acf", 7), ("abcdefg", 8),
            ("abcdfg", 9)
           ]

emap :: (a -> b) -> (Entry a -> Entry b)
emap f (a1, a2) = (map f a1, map f a2)

entry :: Parser (Entry String)
entry =
  do input  <- mapM (const $ sort <$> lexeme (many1 letter)) [1..10]
     symbol "|"
     output <- mapM (const $ sort <$> lexeme (many1 letter)) [1..4]
     return (input, output)

entries :: Parser [Entry String]
entries = many entry >>= \es -> eof >> return es

trivial :: Entry String -> Entry [(String, Int)]
trivial = emap (\s -> [(s, i) | (s', i) <- alphabet, length s == length s'])

solution :: String -> IO ()
solution input =
  print $
  length . map snd . concat .
  map (concat . filter ((==1) . length) . snd . trivial) <$>
  entries `from` input
