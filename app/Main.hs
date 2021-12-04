module Main where

import Year2020.Day2.Puzzle1 (solution)

main :: IO ()
main = lines <$> readFile "./input.txt" >>= solution
