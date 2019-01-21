module Top (repl, processFile, Options (..), defaultOptions) where

import Data.Maybe
import Data.List

import Control.Monad.Trans
import Control.Monad.Except

import System.Console.Haskeline
import System.Console.Pretty

import qualified LLVM.AST as AST

import Misc
import Syntax
import Parse
import Rename
import Typecheck
import Codegen
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

repl :: Options -> IO ()
repl options = do
  putStrLn "---- MyLittleLanguage REPL ----"
  let moduleName = "<stdin>"
  let freshModule = initModule moduleName moduleName
  runInputT defaultSettings $ loop freshModule where
    loop astModule = do
      let moduleName = "<stdin>" -- TODO why is this necessary?
      minput <- getInputLine "➜ "
      case minput of
        Nothing    -> outputStrLn "Goodbye."
        Just ":q"  -> outputStrLn "Goodbye."
        Just input -> do
          newModule <- liftIO $ runExceptT $ process options moduleName astModule input
          case newModule of
            Left err -> do
              liftIO $ print err
              loop astModule
            Right newModule' -> loop newModule'



--------------------------------------------------------------------------------
-- process file
--------------------------------------------------------------------------------

processFile :: Options -> String -> IO Bool
processFile options fileName = do
  source <- readFile fileName
  let moduleName = init $ dropWhileEnd (/= '.') $ fileName
  let freshModule = initModule moduleName fileName
  newModule <- runExceptT $ process options fileName freshModule source
  case newModule of
    Left (err, loc) -> do
      let locString = fromMaybe "" $ fmap locDescription loc
      putStrLn $ "[" ++ color Red "error" ++ "] " ++ locString ++ err
      return False
    Right newModule' -> do
      compile newModule'
      return True



--------------------------------------------------------------------------------
-- common
--------------------------------------------------------------------------------

process :: Options -> String -> AST.Module -> String -> ExceptT Error IO AST.Module
process options name astModule source = do
  parsedModule <- parse name source
  when (_debug options) $ liftIO $ printModule parsedModule
  renamedDefinitions <- rename parsedModule
  typedDefinitions <- typecheckProgram renamedDefinitions
  codegen astModule typedDefinitions

printModule :: Show a => Module a -> IO ()
printModule module_ = printModule' [] module_ where
  printModule' :: Show a => [String] -> Module a -> IO ()
  printModule' modulePath (Module moduleName submodules definitions) = do
    let modulePath' = modulePath ++ [moduleName]
    putStrLn $ "---- Module \"" ++ intercalate "." modulePath' ++ "\" ----"
    mapM_ (putStrLn . (++ "\n") . show) definitions
    mapM_ (printModule' modulePath') submodules

