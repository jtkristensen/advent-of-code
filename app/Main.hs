module Main where

import Year2021.Day2.Puzzle2 (solution)

main :: IO ()
main = lines <$> readFile "./input.txt" >>= solution
