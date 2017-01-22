module Main where

import Control.Monad.Trans

import System.Environment
import System.Console.Haskeline

import Text.Parsec (parse)
import Text.Parsec.String (parseFromFile)


import Parser (myLittleLanguageParser)



main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> repl
    (fileName:_) -> processFile fileName


{- -------- REPL -------- -}
repl :: IO ()
repl = do
  putStrLn "---- MyLittleLanguage REPL ----"
  runInputT defaultSettings loop where
    loop = do
      minput <- getInputLine "➜ "
      case minput of
        Nothing    -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop


{- -------- process file -------- -}
processFile :: String -> IO ()
processFile fileName = do
  fileContent <- readFile fileName
  process fileContent


{- -------- process -------- -}
process :: String -> IO ()
process line = do
  let result = parse myLittleLanguageParser "<stdin>" line
  case result of
    Left  error       -> print error
    Right expressions -> mapM_ print expressions

