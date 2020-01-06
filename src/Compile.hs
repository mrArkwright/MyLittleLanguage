module Compile (compile) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B (fromShort)
import qualified Data.ByteString.Char8 as BC

import Control.Monad.Trans

import System.Process
import System.Directory

import LLVM.Module
import LLVM.Relocation as Relocation
import LLVM.CodeModel as CodeModel
import LLVM.CodeGenOpt as CodeGenOpt
import LLVM.Context
import LLVM.Target
import qualified LLVM.AST as AST

import Paths_MyLittleLanguage
import Misc



compile :: (MonadIO m) => AST.Module -> m ()
compile astModule = liftIO $ withContext $ \context ->

  withModuleFromAST context astModule $ \llvmModule ->

    withHostTargetMachine Relocation.Default CodeModel.Default CodeGenOpt.Default $ \targetMachine -> do

      createDirectoryIfMissing False buildFolder

      let moduleName = BC.unpack $ B.fromShort $ AST.moduleName astModule

      writeLLVMAssemblyToFile (File $ inBuildFolder $ moduleName ++ ".ll") llvmModule

      writeTargetAssemblyToFile targetMachine (File $ inBuildFolder $ moduleName ++ ".s") llvmModule

      bytes <- moduleObject targetMachine llvmModule

      let objectFilePath = inBuildFolder $ moduleName ++ ".o"
      B.writeFile objectFilePath bytes

      builtinsPath <- getDataFileName "rts/builtins.c"
      callProcess "clang" [builtinsPath, "-c", "-o", inBuildFolder "builtins.o"]

      callProcess "ld" ["-e", "_Main.main", "-lSystem", objectFilePath, inBuildFolder "builtins.o", "-o", inBuildFolder moduleName]
