module Compile where

import Control.Monad.Except
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B (toShort, fromShort)
import qualified Data.ByteString.Char8 as BC

import System.Process
import System.Directory

import LLVM.Module
import LLVM.Context
import LLVM.Target
import qualified LLVM as LLVM
import qualified LLVM.AST as AST

import Paths_MyLittleLanguage
import Misc


compile :: AST.Module -> IO ()
compile astModule = withContext $ \context -> do
  withModuleFromAST context astModule $ \llvmModule -> do
    withHostTargetMachine $ \targetMachine -> do

      createDirectoryIfMissing False buildFolder

      let moduleName = BC.unpack $ B.fromShort $ AST.moduleName astModule

      writeLLVMAssemblyToFile (File $ inBuildFolder $ moduleName ++ ".ll") llvmModule

      writeTargetAssemblyToFile targetMachine (File $ inBuildFolder $ moduleName ++ ".s") llvmModule

      bytes <- moduleObject targetMachine llvmModule
      let objectFilePath = inBuildFolder $ moduleName ++ ".o"
      B.writeFile objectFilePath bytes
      builtinsPath <- getDataFileName "rts/builtins.c"
      callProcess "clang" [builtinsPath, "-c", "-o", inBuildFolder "builtins.o"]
      callProcess "ld" ["-demangle", "-dynamic", "-arch", "x86_64", "-macosx_version_min", "10.14.0", "-lSystem", "/usr/local/opt/llvm@5/lib/clang/5.0.1/lib/darwin/libclang_rt.osx.a", objectFilePath, inBuildFolder "builtins.o", "-o", inBuildFolder moduleName]

