{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Language.Haskell.TH
import AdventLib.Parsing
import System.Directory

import Year2021.Day9.Puzzle2

main :: IO ()
main = readFile "./input.txt" >>= solution
