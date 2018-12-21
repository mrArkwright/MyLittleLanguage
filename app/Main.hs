module Main where

import System.Exit
import System.Environment

import Top


main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> repl
    (fileName:_) -> processFile fileName

