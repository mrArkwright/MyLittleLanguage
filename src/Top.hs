module Top (repl, compileFile, Options (..), defaultOptions, Target (..), Compile.compileRuntime) where

import Data.List

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except

import System.Console.Haskeline
import System.Console.Pretty

import qualified LLVM.AST as AST

import Utils
import Parse.Parse
import qualified Parse.Syntax as Parse
import Rename.Rename
import qualified Rename.Syntax as Rename
import Typecheck.Typecheck
import qualified Typecheck.Syntax as Typecheck
import Codegen.Codegen
import qualified Compile



data Options = Options {
    _debug :: Bool,
    _target :: Target
  }

defaultOptions :: Options
defaultOptions = Options {
    _debug = False,
    _target = NativeTarget
  }



--------------------------------------------------------------------------------
-- REPL
--------------------------------------------------------------------------------

repl :: MonadIO m => m ()
repl = do

  liftIO $ putStrLn "---- MyLittleLanguage REPL ----"

  let module_ = newModule replModuleName replSourceName Nothing

  liftIO $ runInputT defaultSettings $ repl' module_


repl' :: MonadException m => AST.Module -> InputT m ()
repl' module_ = do

  input <- getInputLine "➜ "

  case input of
    Nothing -> replGoodbye

    Just ":q" -> replGoodbye

    Just input' -> do
      module_' <- runExceptT $ compileModule defaultOptions replSourceName module_ input'

      case module_' of
        Left err -> do
          printError input' err
          repl' module_

        Right module_'' -> repl' module_''


replGoodbye :: MonadException m => InputT m ()
replGoodbye = outputStrLn "Goodbye."


replModuleName :: String
replModuleName = "Main"


replSourceName :: String
replSourceName = "<interactive>"



--------------------------------------------------------------------------------
-- compile file
--------------------------------------------------------------------------------

compileFile :: MonadIO m => Options -> String -> m Bool
compileFile options fileName = do

  source <- liftIO $ readFile fileName

  let moduleName = init $ dropWhileEnd (/= '.') $ fileName

  let triple = case _target options of
                 NativeTarget -> Nothing
                 EmbeddedTarget triple' _ -> Just triple'
                 ArduinoTarget triple' _ -> Just triple'

  let module_ = newModule moduleName fileName triple

  module_' <- runExceptT $ compileModule options fileName module_ source

  case module_' of
    Left err -> do
      printError source err
      return False

    Right module_'' -> do
      Compile.compile (_target options) module_''
      return True



--------------------------------------------------------------------------------
-- common
--------------------------------------------------------------------------------

compileModule :: (MonadError Error m, MonadIO m) => Options -> String -> AST.Module -> String -> m AST.Module
compileModule options sourceName astModule source = do

  let target = _target options

  parsedModule <- parse sourceName source
  when (_debug options) $ printParsed parsedModule

  renamedDefinitions <- rename target parsedModule
  when (_debug options) $ printRenamed renamedDefinitions

  typedDefinitions <- typecheck target renamedDefinitions
  when (_debug options) $ printTypechecked typedDefinitions

  generatedModule <- codegen target astModule typedDefinitions
  when (_debug options) $ printCodeGenerated generatedModule

  return generatedModule


printParsed :: MonadIO m => Parse.Module -> m ()
printParsed module_ = do

  liftIO $ putStrLn "---- Parsed ----"

  printModule [] module_ where

  printModule :: MonadIO m => [String] -> Parse.Module -> m ()
  printModule modulePath (Parse.Module moduleName submodules definitions) = do

    let modulePath' = modulePath -:+ moduleName

    liftIO $ putStrLn $ "- Module " ++ intercalate "." modulePath' ++ ":"

    liftIO $ mapM_ (putStrLn . (++ "\n") . show) definitions
    liftIO $ mapM_ (printModule modulePath') submodules


printRenamed :: MonadIO m => [Rename.GlobalDefinition] -> m ()
printRenamed definitions = do
  liftIO $ putStrLn "---- Renamed ----"
  liftIO $ mapM_ (putStrLn . (++ "\n") . show) definitions


printTypechecked :: MonadIO m => [Typecheck.GlobalDefinition] -> m ()
printTypechecked definitions = do
  liftIO $ putStrLn "---- Typecheckd ----"
  liftIO $ mapM_ (putStrLn . (++ "\n") . show) definitions


printCodeGenerated :: MonadIO m => AST.Module -> m ()
printCodeGenerated module_ = do
  liftIO $ putStrLn "---- Module ----"
  liftIO $ print module_
  liftIO $ putStrLn ""


printError :: MonadIO m => String -> Error -> m ()
printError _ (err, phase, Nothing) = do
  liftIO $ putStr $ style Bold $ color Red "error: "
  liftIO $ putStr $ color Cyan $ "[" ++ show phase ++ "] "
  liftIO $ putStrLn $ style Bold $ color White err

printError source (err, phase, Just loc) = do
  let startLine = loc_startLine loc
  let endLine = loc_endLine loc

  let affectedLine = (lines source) !! (startLine - 1)

  let startColumn = loc_startColumn loc
  let endColumn = if (startLine == endLine) then loc_endColumn loc else length affectedLine + 1

  liftIO $ putStr $ style Bold $ loc_sourceName loc ++ ":" ++ show startLine ++ ":" ++ show startColumn
  when (startColumn /= endColumn) $ liftIO $ putStr $ style Bold $ "-" ++ show endColumn
  liftIO $ putStr $ style Bold ": "
  liftIO $ putStr $ style Bold $ color Red "error: "
  liftIO $ putStr $ color Cyan $ "[" ++ show phase ++ "] "
  liftIO $ putStrLn $ style Bold $ color White err

  liftIO $ putStrLn $ affectedLine

  let errorLength = if (endColumn > startColumn) then endColumn - startColumn else 1
  liftIO $ putStrLn $ color Yellow $ replicate (startColumn - 1) ' ' ++ replicate errorLength '^'
