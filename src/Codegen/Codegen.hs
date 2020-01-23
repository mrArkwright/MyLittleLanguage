module Codegen.Codegen (initModule, codegen) where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.ByteString.Short as B (toShort)
import qualified Data.ByteString.Char8 as BC

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Global as LLVM

import Misc
import RuntimeSystem (libraryBuiltins)
import Parse.Syntax (Type(..), Parameter(..))
import Rename.Syntax (Symbol(..), GlobalSymbol(..))
import Typecheck.Syntax
import Codegen.Lib
import Codegen.CodegenFunction



--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

initModule :: String -> String -> LLVM.Module
initModule name fileName = LLVM.defaultModule {
    LLVM.moduleName = B.toShort $ BC.pack name,
    LLVM.moduleSourceFileName = B.toShort $ BC.pack fileName
  }


codegen :: MonadError Error m => LLVM.Module -> [GlobalDefinition] -> m LLVM.Module
codegen astModule definitions = evalStateT' (Codegen astModule M.empty) $ do

  mapM_ importLibraryBuiltin $ M.toList libraryBuiltins
  mapM_ importGlobalDefinition definitions

  mapM_ codegenLibraryBuiltin $ M.toList libraryBuiltins
  mapM_ codegenGlobalDefinition definitions

  gets codegen_module


importLibraryBuiltin :: (MonadState Codegen m, MonadError Error m) => (GlobalSymbol, Type) -> m ()
importLibraryBuiltin (symbol, type_) = do
  let symbol' = SymbolGlobal $ symbol
  operand <- constantOperand symbol' type_
  addToSymbolTable symbol' type_ operand


importGlobalDefinition :: (MonadState Codegen m, MonadError Error m) => GlobalDefinition -> m ()
importGlobalDefinition definition = do
  let symbol = SymbolGlobal $ globalDefinitionSymbol definition
  let type_ = globalDefinitionType definition
  operand <- constantOperand symbol type_
  addToSymbolTable symbol type_ operand


codegenLibraryBuiltin :: (MonadState Codegen m, MonadError Error m) => (GlobalSymbol, Type) -> m ()
codegenLibraryBuiltin (symbol, type_) = case type_ of

  TypeFunction parameterTypes resultType -> do
    let namedParameters = map (\(parameterType, i) -> Parameter ("x" ++ show i) parameterType) $ zipWithIndex parameterTypes
    addGlobalFunction symbol namedParameters resultType []

  _ -> throwError ("(Codegen) codegenLibraryBuiltin not implemented for type " ++ show type_, Nothing)


codegenGlobalDefinition :: (MonadState Codegen m, MonadError Error m) => GlobalDefinition -> m ()
codegenGlobalDefinition (GlobalDefinitionValue definition) = do
  throwError ("(Codegen) codegenDefinition not implemented for global values.", Just $ globalValueDefinition_loc definition)

codegenGlobalDefinition (GlobalDefinitionFunction definition) = do

  symbolTable <- gets codegen_symbolTable

  let symbol = functionDefinition_symbol definition
  let parameters = functionDefinition_parameters definition
  let resultType = functionDefinition_resultType definition

  basicBlocks <- codegenFunction symbolTable definition
  addGlobalFunction symbol parameters resultType basicBlocks


addGlobalFunction :: (MonadState Codegen m, MonadError Error m) => GlobalSymbol -> [Parameter] -> Type -> [LLVM.BasicBlock] -> m ()
addGlobalFunction symbol parameters resultType basicBlocks = do

  parameters' <- forM parameters $ \parameter -> do
    parameterType <- typeToLlvmType $ parameter_type parameter
    return $ LLVM.Parameter parameterType (LLVM.Name $ B.toShort $ BC.pack $ parameter_name parameter) []

  resultType' <- typeToLlvmType resultType

  let llvmDefinition = LLVM.GlobalDefinition $ LLVM.functionDefaults {
    LLVM.name        = LLVM.Name (B.toShort $ BC.pack $ show symbol),
    LLVM.parameters  = (parameters', False),
    LLVM.returnType  = resultType',
    LLVM.basicBlocks = basicBlocks
  }

  addToModuleDefinitions llvmDefinition



--------------------------------------------------------------------------------
-- Codegen primitives and data definition
--------------------------------------------------------------------------------

addToModuleDefinitions :: (MonadState Codegen m, MonadError Error m) => LLVM.Definition -> m ()
addToModuleDefinitions definition = do
  module_ <- gets codegen_module
  let moduleDefinitions = LLVM.moduleDefinitions module_
  modify $ \s -> s { codegen_module = module_ { LLVM.moduleDefinitions = moduleDefinitions -:+ definition } }


addToSymbolTable :: (MonadState Codegen m, MonadError Error m) => Symbol -> Type -> LLVM.Operand -> m ()
addToSymbolTable symbol type_ operand = do
  symbolTable <- gets codegen_symbolTable
  modify $ \s -> s { codegen_symbolTable = M.insert symbol (type_, operand) symbolTable }


data Codegen = Codegen {
  codegen_module :: LLVM.Module,
  codegen_symbolTable :: SymbolTable
}
