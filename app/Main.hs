module Main where
import Year2021.Day13.Puzzle1
main :: IO ()
main = readFile "./input.txt" >>= solution
