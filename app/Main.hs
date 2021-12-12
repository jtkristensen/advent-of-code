{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Language.Haskell.TH
import AdventLib.Parsing
import System.Directory

import Year2021.Day12.Puzzle1

main :: IO ()
main = readFile "./example.txt" >>= solution
