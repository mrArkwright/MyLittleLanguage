module Main where

import Control.Monad.Trans
import Control.Monad.Except

import System.Exit
import System.Environment
import System.Console.GetOpt

import qualified Top


data Options = Options {
    _debug :: Bool,
    _target :: String,
    _triple :: Maybe String,
    _cpu :: Maybe String
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
    _debug = Top._debug Top.defaultOptions,
    _target = "native",
    _triple = Nothing,
    _cpu = Nothing
  }

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = [
    Option ['d'] ["debug"] (NoArg (\opts -> opts { _debug = True })) "debug mode",
    Option [] ["target"] (ReqArg (\target opts -> opts { _target = target }) "<arg>") "",
    Option [] ["triple"] (ReqArg (\triple opts -> opts { _triple = Just triple }) "<arg>") "",
    Option [] ["cpu"] (ReqArg (\cpu opts -> opts { _cpu = Just cpu }) "<arg>") ""
  ]

main :: IO ()
main = do

  result <- runExceptT main'

  case result of
    Left err -> do
      putStrLn err
      exitFailure

    Right () ->
      exitSuccess


main' :: (MonadError String m, MonadIO m) => m ()
main' = do

  args <- liftIO getArgs

  (options, fileNames) <- case getOpt Permute optionDescriptions args of
    (o, n, [])   -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> throwError (concat errs ++ usageInfo "Usage: ..." optionDescriptions)

  target <- case _target options of
    "native" -> return Top.NativeTarget

    "embedded" -> do
      triple <- maybeToError (_triple options) "triple must be specified when using target embedded"
      cpu <- maybeToError (_cpu options) "cpu must be specified when using target embedded"
      return $ Top.EmbeddedTarget triple cpu

    "arduino" -> do
      triple <- maybeToError (_triple options) "triple must be specified when using target arduino"
      cpu <- maybeToError (_cpu options) "cpu must be specified when using target arduino"
      return $ Top.ArduinoTarget triple cpu

    target' -> throwError $ "invalid target: " ++ target'

  let processOptions = Top.defaultOptions {
      Top._debug = _debug options,
      Top._target = target
    }

  case fileNames of
    [] -> Top.repl processOptions

    (fileName : _) -> do
      success <- Top.processFile processOptions fileName
      if (success) then return () else throwError "compilation failed"


maybeToError :: MonadError e m => Maybe a -> e -> m a
maybeToError (Just x) _ = return x
maybeToError Nothing err = throwError err
