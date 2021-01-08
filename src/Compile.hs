module Compile (compile, compileRuntime) where

import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B (fromShort, toShort, ShortByteString)
import qualified Data.ByteString.Char8 as BC

import Control.Monad.Trans

import System.Process
import System.Directory

import LLVM.Module
import LLVM.Relocation as Relocation
import LLVM.CodeModel as CodeModel
import LLVM.CodeGenOpt as CodeGenOpt
import LLVM.Context
import LLVM.Target (withTargetOptions, initializeAllTargets, lookupTarget, withTargetMachine, getProcessTargetTriple, getHostCPUName, getHostCPUFeatures, CPUFeature)
import qualified LLVM.Target as LLVM (Target)
import LLVM.PassManager (withPassManager, runPassManager)
import qualified LLVM.PassManager as PassManager
import qualified LLVM.AST as AST
import LLVM.Transforms ( Pass(..) )

import Paths_MyLittleLanguage
import Utils



compile :: (MonadIO m) => Target -> AST.Module -> m ()
compile target astModule = liftIO $ withContext $ \context ->

  withModuleFromAST context astModule $ \llvmModule ->

     withPassManager optimizationPasses $ \passManager -> do

      _ <- runPassManager passManager llvmModule

      createDirectoryIfMissing False buildFolder

      initializeAllTargets

      (triple, cpu, features, target') <- detailsForTarget target

      withTargetOptions $ \targetOptions ->

        withTargetMachine target' triple cpu features targetOptions Relocation.Default CodeModel.Default CodeGenOpt.Default $ \targetMachine -> do

          let moduleName = BC.unpack $ B.fromShort $ AST.moduleName astModule

          writeLLVMAssemblyToFile (File $ inBuildFolder $ moduleName ++ ".ll") llvmModule

          writeTargetAssemblyToFile targetMachine (File $ inBuildFolder $ moduleName ++ ".s") llvmModule

          bytes <- moduleObject targetMachine llvmModule
          let objectFilePath = inBuildFolder $ moduleName ++ ".o"
          B.writeFile objectFilePath bytes


optimizationPasses :: PassManager.PassSetSpec
optimizationPasses = PassManager.defaultPassSetSpec { PassManager.transforms = [GlobalValueNumbering True, TailCallElimination] }


detailsForTarget :: MonadIO m => Target -> m (B.ShortByteString, BC.ByteString, M.Map CPUFeature Bool, LLVM.Target)
detailsForTarget NativeTarget = liftIO $ do
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  return (triple, cpu, features, target)

detailsForTarget (EmbeddedTarget triple cpu) = liftIO $ do
  let triple' = B.toShort $ BC.pack triple
  let cpu' = BC.pack cpu
  let features = M.empty
  (target, _) <- lookupTarget Nothing triple'
  return (triple', cpu', features, target)

detailsForTarget (ArduinoTarget triple cpu) = liftIO $ do
  let triple' = B.toShort $ BC.pack triple
  let cpu' = BC.pack cpu
  let features = M.empty
  (target, _) <- lookupTarget Nothing triple'
  return (triple', cpu', features, target)


compileRuntime :: MonadIO m => Target -> m ()
compileRuntime NativeTarget = liftIO $ do
  initializeAllTargets
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName

  runtimePath <- getDataFileName "runtime/runtime_native.ll"
  callProcess "llc-9" ["-mtriple=" ++ BC.unpack (B.fromShort triple), "-mcpu=" ++ BC.unpack cpu, "-filetype=obj", runtimePath, "-o", inBuildFolder "runtime_native.o"]

compileRuntime (EmbeddedTarget _ _) = return ()

compileRuntime (ArduinoTarget triple cpu) = liftIO $ do
  runtimePath <- getDataFileName "runtime/runtime_arduino.ll"
  callProcess "llc-9" ["-mtriple=" ++ triple, "-mcpu=" ++ cpu, "-filetype=obj", runtimePath, "-o", inBuildFolder "runtime_arduino.o"]
