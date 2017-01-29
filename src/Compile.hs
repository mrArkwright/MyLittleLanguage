module Compile where

import Control.Monad.Except
import qualified Data.ByteString as B
import Paths_MyLittleLanguage
import System.Process

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
        Right bytes -> do
          let objectFileName = moduleName ++ ".o"
          B.writeFile objectFileName bytes
          builtinsPath <- getDataFileName "rts/builtins.c"
          callProcess "clang" [builtinsPath, "-c", "-o", "builtins.o"]
          callProcess "ld" ["-demangle", "-dynamic", "-arch", "x86_64", "-macosx_version_min", "10.11.0", "-lSystem", "/usr/local/lib/clang/3.9.1/lib/darwin/libclang_rt.osx.a", objectFileName, "builtins.o", "-o", "main"]

    case result of
      Left error -> putStrLn error
      Right ()   -> return ()

  case result of
    Left error -> putStrLn error
    Right ()   -> return ()

