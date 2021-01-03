module Top (repl, processFile, Options (..), defaultOptions, Target (..)) where

import Data.Maybe
import Data.List

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except

import System.Console.Haskeline
import System.Console.Pretty

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as B (fromShort)

import qualified LLVM.AST as AST

import Misc
import Parse.Parse
import qualified Parse.Syntax as Parse
import Rename.Rename
import qualified Rename.Syntax as Rename
import Typecheck.Typecheck
import qualified Typecheck.Syntax as Typecheck
import Codegen.Codegen
import Compile



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

repl :: (MonadIO m) => Options -> m ()
repl options = do

  liftIO $ putStrLn "---- MyLittleLanguage REPL ----"

  let moduleName = "<stdin>"
  let module_ = newModule moduleName moduleName Nothing

  liftIO $ runInputT defaultSettings $ loop module_ where

    loop module_' = do

      input <- getInputLine "➜ "

      case input of

        Nothing    -> outputStrLn "Goodbye."

        Just ":q"  -> outputStrLn "Goodbye."

        Just input' -> do

          let moduleName = BC.unpack $ B.fromShort $ AST.moduleName module_'
          module_'' <- runExceptT $ process options moduleName module_' input'

          case module_'' of

            Left err -> do
              liftIO $ print err
              loop module_'

            Right module_''' -> loop module_'''



--------------------------------------------------------------------------------
-- process file
--------------------------------------------------------------------------------

processFile :: (MonadIO m) => Options -> String -> m Bool
processFile options fileName = do

  source <- liftIO $ readFile fileName

  let moduleName = init $ dropWhileEnd (/= '.') $ fileName

  let triple = case _target options of
                 NativeTarget -> Nothing
                 EmbeddedTarget triple' _ -> Just triple'
                 ArduinoTarget triple' _ -> Just triple'

  let module_ = newModule moduleName fileName triple

  module_' <- runExceptT $ process options fileName module_ source

  case module_' of

    Left (err, loc) -> do
      let locString = fromMaybe "" $ fmap locDescription loc
      liftIO $ putStrLn $ "[" ++ color Red "error" ++ "] " ++ locString ++ err
      return False

    Right module_'' -> do
      compile (_target options) module_''
      return True



--------------------------------------------------------------------------------
-- common
--------------------------------------------------------------------------------

process :: (MonadError Error m, MonadIO m) => Options -> String -> AST.Module -> String -> m AST.Module
process options name astModule source = do

  parsedModule <- parse name source
  when (_debug options) $ printParsed parsedModule

  renamedDefinitions <- rename parsedModule
  when (_debug options) $ printRenamed renamedDefinitions

  typedDefinitions <- typecheck renamedDefinitions
  when (_debug options) $ printTypechecked typedDefinitions

  generatedModule <- codegen (_target options) astModule typedDefinitions
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

