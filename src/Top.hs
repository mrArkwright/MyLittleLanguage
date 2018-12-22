module Top where

import Data.List

import Control.Monad.Trans

import System.Console.Haskeline
import System.Console.Pretty

import Text.Parsec (parse)

import qualified LLVM.AST as AST

import Parser (myLittleLanguageParser)
import Codegen
import Compile


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
  let program = parse myLittleLanguageParser "<stdin>" source
  case program of
    Left  err         -> print err >> return Nothing
    Right definitions -> do
      --putStrLn "---- Definitions ----"
      --mapM_ (putStrLn . (++ "\n") . show) definitions

      return $ Just $ codegen astModule definitions

