{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Language.Haskell.TH
import AdventLib.Parsing
import System.Directory

-- import Year2021.Day3.Puzzle1 (solution)

import Year2021.Day3.Puzzle2

main :: IO ()
main = lines <$> readFile "./input.txt" >>= solution
