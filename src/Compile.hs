module Compile where

import Control.Monad.Except
import qualified Data.ByteString as B
--import System.Cmd

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.Target
import qualified LLVM.General as LLVM
import qualified LLVM.General.AST as AST


compile :: AST.Module -> IO ()
compile astModule = withContext $ \context -> do
  result <- runExceptT $ withModuleFromAST context astModule $ \llvmModule -> do
    result <- runExceptT $ withHostTargetMachine $ \targetMachine -> do

      let moduleName = AST.moduleName astModule

      llvmAssembly <- moduleLLVMAssembly llvmModule
      writeFile (moduleName ++ ".ll") llvmAssembly

      result <- runExceptT $ moduleTargetAssembly targetMachine llvmModule
      case result of
        Left error     -> putStrLn error
        Right assembly -> writeFile (moduleName ++ ".s") assembly

      result <- runExceptT $ moduleObject targetMachine llvmModule
      case result of
        Left error  -> putStrLn error
        Right bytes -> B.writeFile (moduleName ++ ".o") bytes

    case result of
      Left error -> putStrLn error
      Right ()   -> return ()

  case result of
    Left error -> putStrLn error
    Right ()   -> return ()

