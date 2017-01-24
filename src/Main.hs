module Main where

import Control.Monad.Trans
import Data.List
import System.Environment
import System.Console.Haskeline

import Text.Parsec (parse)
import Text.Parsec.String (parseFromFile)

import qualified LLVM.General.AST as AST


import Parser (myLittleLanguageParser)
import Codegen
import Compile



main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> repl
    (fileName:_) -> processFile fileName



--------------------------------------------------------------------------------
-- REPL
--------------------------------------------------------------------------------

repl :: IO ()
repl = do
  putStrLn "---- MyLittleLanguage REPL ----"
  runInputT defaultSettings (loop $ initModule "repl") where
    loop astModule = do
      minput <- getInputLine "➜ "
      case minput of
        Nothing    -> outputStrLn "Goodbye."
        Just ":q"  -> outputStrLn "Goodbye."
        Just input -> do
          newModule <- liftIO $ process astModule input
          case newModule of
            Nothing        -> loop astModule
            Just newModule -> loop newModule



--------------------------------------------------------------------------------
-- process file
--------------------------------------------------------------------------------

processFile :: String -> IO ()
processFile fileName = do
  source <- readFile fileName
  let moduleName = init $ dropWhileEnd (/= '.') $ fileName
  newModule <- process (initModule moduleName) source
  case newModule of
    Nothing -> return ()
    Just newModule -> compile newModule



--------------------------------------------------------------------------------
-- common
--------------------------------------------------------------------------------

process :: AST.Module -> String -> IO (Maybe AST.Module)
process astModule source = do
  let program = parse myLittleLanguageParser "<stdin>" source
  case program of
    Left  error       -> print error >> return Nothing
    Right definitions -> do
      --putStrLn "---- Definitions ----"
      --mapM_ (putStrLn . (++ "\n") . show) definitions

      return $ Just $ codegen astModule definitions


initModule :: String -> AST.Module
initModule name = AST.defaultModule { AST.moduleName = name }

