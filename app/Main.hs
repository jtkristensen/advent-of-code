module Main where
import Year2021.Day14.Puzzle2
main :: IO ()
main = readFile "./input.txt" >>= solution
