module Main where
import Year2021.Day03.Puzzle2
main :: IO ()
main = readFile "input.txt" >>= solution
