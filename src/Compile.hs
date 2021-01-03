module Compile (compile, compileEmbedded) where

import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B (fromShort, toShort)
import qualified Data.ByteString.Char8 as BC

import Control.Monad.Trans

import System.Process
import System.Directory

import LLVM.Module
import LLVM.Relocation as Relocation
import LLVM.CodeModel as CodeModel
import LLVM.CodeGenOpt as CodeGenOpt
import LLVM.Context
import LLVM.Target (withHostTargetMachine, withTargetOptions, initializeAllTargets, lookupTarget, withTargetMachine)
import LLVM.PassManager (withPassManager, runPassManager)
import qualified LLVM.PassManager as PassManager
import qualified LLVM.AST as AST
import LLVM.Transforms ( Pass(..) )

import Paths_MyLittleLanguage
import Misc



compile :: (MonadIO m) => Target -> AST.Module -> m ()
compile NativeTarget astModule = compileNative astModule
compile (EmbeddedTarget triple cpu) astModule = compileEmbedded triple cpu astModule
compile (ArduinoTarget triple cpu) astModule = compileArduino triple cpu astModule


compileNative :: (MonadIO m) => AST.Module -> m ()
compileNative astModule = liftIO $ withContext $ \context ->

  withModuleFromAST context astModule $ \llvmModule ->

     withPassManager optimizationPasses $ \passManager -> do

      _ <- runPassManager passManager llvmModule

      withHostTargetMachine Relocation.Default CodeModel.Default CodeGenOpt.Default $ \targetMachine -> do

        createDirectoryIfMissing False buildFolder

        let moduleName = BC.unpack $ B.fromShort $ AST.moduleName astModule

        writeLLVMAssemblyToFile (File $ inBuildFolder $ moduleName ++ ".ll") llvmModule

        writeTargetAssemblyToFile targetMachine (File $ inBuildFolder $ moduleName ++ ".s") llvmModule

        bytes <- moduleObject targetMachine llvmModule

        let objectFilePath = inBuildFolder $ moduleName ++ ".o"
        B.writeFile objectFilePath bytes

        runtimePath <- getDataFileName "runtime/runtime_native.c"
        callProcess "clang" [runtimePath, "-c", "-o", inBuildFolder "builtins.o"]

        callProcess "ld" ["-e", "_Main.main", "-lSystem", objectFilePath, inBuildFolder "builtins.o", "-o", inBuildFolder moduleName]


compileEmbedded :: (MonadIO m) => String -> String -> AST.Module -> m ()
compileEmbedded triple cpu astModule = liftIO $ withContext $ \context ->

  withModuleFromAST context astModule $ \llvmModule ->
  
    withTargetOptions $ \targetOptions ->

      withPassManager optimizationPasses $ \passManager -> do

        _ <- runPassManager passManager llvmModule

        createDirectoryIfMissing False buildFolder

        initializeAllTargets
        let triple' = B.toShort $ BC.pack triple
        (target, _) <- lookupTarget Nothing triple'
        let cpu' = BC.pack cpu
        let features = M.empty

        withTargetMachine target triple' cpu' features targetOptions Relocation.Default CodeModel.Default CodeGenOpt.None $ \targetMachine -> do

          let moduleName = BC.unpack $ B.fromShort $ AST.moduleName astModule

          writeLLVMAssemblyToFile (File $ inBuildFolder $ moduleName ++ ".ll") llvmModule

          writeTargetAssemblyToFile targetMachine (File $ inBuildFolder $ moduleName ++ ".s") llvmModule

          bytes <- moduleObject targetMachine llvmModule
          let objectFileName = inBuildFolder $ moduleName ++ ".o"
          B.writeFile objectFileName bytes


compileArduino :: (MonadIO m) => String -> String -> AST.Module -> m ()
compileArduino triple cpu astModule = liftIO $ withContext $ \context ->

  withModuleFromAST context astModule $ \llvmModule ->

    withTargetOptions $ \targetOptions ->

      withPassManager optimizationPasses $ \passManager -> do

        _ <- runPassManager passManager llvmModule

        createDirectoryIfMissing False buildFolder

        initializeAllTargets
        let triple' = B.toShort $ BC.pack triple
        (target, _) <- lookupTarget Nothing triple'
        let cpu' = BC.pack cpu
        let features = M.empty

        withTargetMachine target triple' cpu' features targetOptions Relocation.Default CodeModel.Default CodeGenOpt.None $ \targetMachine -> do

          let moduleName = BC.unpack $ B.fromShort $ AST.moduleName astModule

          writeLLVMAssemblyToFile (File $ inBuildFolder $ moduleName ++ ".ll") llvmModule

          writeTargetAssemblyToFile targetMachine (File $ inBuildFolder $ moduleName ++ ".s") llvmModule

          bytes <- moduleObject targetMachine llvmModule
          let objectFileName = inBuildFolder $ moduleName ++ ".o"
          B.writeFile objectFileName bytes

          runtimePath <- getDataFileName "runtime/runtime_arduino.ll"
          callProcess "llc-9" ["-mtriple=" ++ triple, "-mcpu=" ++ cpu, "-filetype=obj", runtimePath, "-o", inBuildFolder "runtime_arduino.o"]


optimizationPasses :: PassManager.PassSetSpec
optimizationPasses = PassManager.defaultPassSetSpec { PassManager.transforms = [GlobalValueNumbering True, TailCallElimination] }
