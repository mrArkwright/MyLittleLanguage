module Main where

import System.Exit
import System.Environment
import System.Console.GetOpt

import qualified Top


data Options = Options {
    _debug :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
    _debug = Top._debug Top.defaultOptions
  }

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = [
    Option ['d'] ["debug"] (NoArg (\opts -> opts { _debug = True })) "debug mode"
  ]

main :: IO ()
main = do
  args <- getArgs
  (options, fileNames) <- case getOpt Permute optionDescriptions args of
    (o, n, [])   -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo "Usage: ..." optionDescriptions))
  let processOptions = Top.defaultOptions {
      Top._debug = _debug options
    }
  case fileNames of
    []             -> Top.repl processOptions
    (fileName : _) -> do
      success <- Top.processFile processOptions fileName
      if (success) then exitSuccess else exitFailure

