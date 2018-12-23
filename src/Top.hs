module Top (repl, processFile) where

import Data.List

import Control.Monad.Trans
import Control.Monad.Except

import System.Console.Haskeline
import System.Console.Pretty

import qualified LLVM.AST as AST

import Syntax
import Parser
import Codegen
import Compile


debug :: Bool
debug = True

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
            Just newModule' -> loop newModule'



--------------------------------------------------------------------------------
-- process file
--------------------------------------------------------------------------------

processFile :: String -> IO Bool
processFile fileName = do
  source <- readFile fileName
  let moduleName = init $ dropWhileEnd (/= '.') $ fileName
  newModule <- process (initModule moduleName) source
  case newModule of
    Nothing -> do
      putStrLn $ "[" ++ color Red "error" ++ "] "
      return False
    Just newModule' -> do
      compile newModule'
      return True



--------------------------------------------------------------------------------
-- common
--------------------------------------------------------------------------------

process :: AST.Module -> String -> IO (Maybe AST.Module)
process astModule source = do
  let program = parse "<stdin>" source
  case program of
    Left  err         -> print err >> return Nothing
    Right definitions -> do
      when debug $ printDefinitions definitions
      return $ Just $ codegen astModule definitions

printDefinitions :: [Def] -> IO ()
printDefinitions definitions = do
  putStrLn "---- Definitions ----"
  mapM_ (putStrLn . (++ "\n") . show) definitions

