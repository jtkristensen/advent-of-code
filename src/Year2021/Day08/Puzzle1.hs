module Year2021.Day08.Puzzle1 where

import AdventLib.Parsing
import Data.List      ( sort  )
import Data.Bifunctor ( bimap )

type Entry a = ([a], [a])

alphabet :: [(String, Int)]
alphabet =
  [ ("abcefg" , 0), ("cf"    , 1)
  , ("acdeg"  , 2), ("acdfg" , 3)
  , ("bcdf"   , 4), ("abdfg" , 5)
  , ("abdefg" , 6), ("acf"   , 7)
  , ("abcdefg", 8), ("abcdfg", 9)
  ]

lift :: (a -> b) -> (Entry a -> Entry b)
lift f = bimap (fmap f) (fmap f)

entries :: Parser [Entry String]
entries = many ((,) <$> readings 10 <*> (symbol "|" *> readings 4)) <* eof
  where readings n = mapM (const $ sort <$> lexeme (many1 letter)) [1..n]

trivial :: Entry String -> Entry [(String, Int)]
trivial = lift $ \s -> filter ((==length s) . length . fst) alphabet

solution :: String -> IO ()
solution input =
  print $
  length . map snd . concat .
  map (concat . filter ((==1) . length) . snd . trivial) <$>
  entries `from` input
