{-# LANGUAGE TemplateHaskell #-}

module Main where

import Year2021.Day12.Puzzle2

main :: IO ()
main = readFile "./input.txt" >>= solution

