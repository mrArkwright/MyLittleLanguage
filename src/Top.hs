module Top (repl, processFile) where

import Data.List

import Control.Monad.Trans
import Control.Monad.Except

import System.Console.Haskeline
import System.Console.Pretty

import qualified LLVM.AST as AST

import Misc
import Syntax
import Parser
import Typecheck
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
          newModule <- liftIO $ runExceptT $ process astModule input
          case newModule of
            Left err -> do
              liftIO $ print err
              loop astModule
            Right newModule' -> loop newModule'



--------------------------------------------------------------------------------
-- process file
--------------------------------------------------------------------------------

processFile :: String -> IO Bool
processFile fileName = do
  source <- readFile fileName
  let moduleName = init $ dropWhileEnd (/= '.') $ fileName
  newModule <- runExceptT $ process (initModule moduleName) source
  case newModule of
    Left err -> do
      putStrLn $ "[" ++ color Red "error" ++ "] " ++ err
      return False
    Right newModule' -> do
      compile newModule'
      return True



--------------------------------------------------------------------------------
-- common
--------------------------------------------------------------------------------

process :: AST.Module -> String -> ExceptT Error IO AST.Module
process astModule source = do
  definitions <- parse "<stdin>" source
  when debug $ liftIO $ printDefinitions definitions
  typecheckProgram definitions
  codegen astModule definitions

printDefinitions :: [Def] -> IO ()
printDefinitions definitions = do
  putStrLn "---- Definitions ----"
  mapM_ (putStrLn . (++ "\n") . show) definitions

