module Main.CompileRuntime (compileRuntime) where

import Control.Monad.Trans
import Control.Monad.Except

import System.Console.GetOpt

import qualified Top
import Main.Utils


data Options = Options {
    _target :: String,
    _triple :: Maybe String,
    _cpu :: Maybe String
  } deriving Show


defaultOptions :: Options
defaultOptions = Options {
    _target = "native",
    _triple = Nothing,
    _cpu = Nothing
  }


optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions = [
    Option [] ["target"] (ReqArg (\target opts -> opts { _target = target }) "<arg>") "",
    Option [] ["triple"] (ReqArg (\triple opts -> opts { _triple = Just triple }) "<arg>") "",
    Option [] ["cpu"] (ReqArg (\cpu opts -> opts { _cpu = Just cpu }) "<arg>") ""
  ]


compileRuntime :: (MonadError String m, MonadIO m) => [String] -> m ()
compileRuntime args = do

  (options, _) <- case getOpt Permute optionDescriptions args of
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

  Top.compileRuntime target

  liftIO $ putStrLn "successfully compiled runtime!"
