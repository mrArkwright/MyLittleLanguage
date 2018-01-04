module Compile where

import Control.Monad.Except
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B (toShort, fromShort)
import qualified Data.ByteString.Char8 as BC
import Paths_MyLittleLanguage
import System.Process

import LLVM.Module
import LLVM.Context
import LLVM.Target
import qualified LLVM as LLVM
import qualified LLVM.AST as AST


compile :: AST.Module -> IO ()
compile astModule = withContext $ \context -> do
  withModuleFromAST context astModule $ \llvmModule -> do
    withHostTargetMachine $ \targetMachine -> do

      let moduleName = BC.unpack $ B.fromShort $ AST.moduleName astModule

      writeLLVMAssemblyToFile (File $ moduleName ++ ".ll") llvmModule

      writeTargetAssemblyToFile targetMachine (File $ moduleName ++ ".s") llvmModule

      bytes <- moduleObject targetMachine llvmModule
      let objectFileName = moduleName ++ ".o"
      B.writeFile objectFileName bytes
      builtinsPath <- getDataFileName "rts/builtins.c"
      callProcess "clang" [builtinsPath, "-c", "-o", "builtins.o"]
      callProcess "ld" ["-demangle", "-dynamic", "-arch", "x86_64", "-macosx_version_min", "10.13.0", "-lSystem", "/usr/local/opt/llvm@4/lib/clang/4.0.1/lib/darwin/libclang_rt.osx.a", objectFileName, "builtins.o", "-o", "main"]

