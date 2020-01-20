module Top (repl, processFile, Options (..), defaultOptions) where

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
    _debug :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
    _debug = False
  }



--------------------------------------------------------------------------------
-- REPL
--------------------------------------------------------------------------------

repl :: (MonadIO m) => Options -> m ()
repl options = do

  liftIO $ putStrLn "---- MyLittleLanguage REPL ----"

  let moduleName = "<stdin>"
  let freshModule = initModule moduleName moduleName

  liftIO $ runInputT defaultSettings $ loop freshModule where

    loop astModule = do

      input <- getInputLine "➜ "

      case input of

        Nothing    -> outputStrLn "Goodbye."

        Just ":q"  -> outputStrLn "Goodbye."

        Just input' -> do

          let moduleName = BC.unpack $ B.fromShort $ AST.moduleName astModule
          newModule <- runExceptT $ process options moduleName astModule input'

          case newModule of

            Left err -> do
              liftIO $ print err
              loop astModule

            Right newModule' -> loop newModule'



--------------------------------------------------------------------------------
-- process file
--------------------------------------------------------------------------------

processFile :: (MonadIO m) => Options -> String -> m Bool
processFile options fileName = do

  source <- liftIO $ readFile fileName

  let moduleName = init $ dropWhileEnd (/= '.') $ fileName
  let freshModule = initModule moduleName fileName

  newModule <- runExceptT $ process options fileName freshModule source

  case newModule of

    Left (err, loc) -> do
      let locString = fromMaybe "" $ fmap locDescription loc
      liftIO $ putStrLn $ "[" ++ color Red "error" ++ "] " ++ locString ++ err
      return False

    Right newModule' -> do
      compile newModule'
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

  codegen astModule typedDefinitions


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
