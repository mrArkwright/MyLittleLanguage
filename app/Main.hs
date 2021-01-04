module Main where

import Control.Monad.Trans
import Control.Monad.Except

import System.Exit
import System.Environment

import Compile
import CompileRuntime
import Repl


main :: IO ()
main = do

  args <- getArgs
  result <- runExceptT $ main' args

  case result of
    Left err -> do
      putStrLn $ "error: " ++ err
      exitFailure

    Right () ->
      exitSuccess


main' :: (MonadError String m, MonadIO m) => [String] -> m ()
main' ("compile" : args) = compile args
main' ("compile-runtime" : args) = compileRuntime args
main' ("repl" : args) = repl args
main' (command : _) = throwError $ "unknown command: " ++ command
main' [] = throwError "no command specified"
