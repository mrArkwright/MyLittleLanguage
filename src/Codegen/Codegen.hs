module Codegen.Codegen (newModule, codegen) where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.ByteString.Short as B (toShort)
import qualified Data.ByteString.Char8 as BC

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Global as LLVM.Global
import qualified LLVM.AST.DataLayout as LLVM

import Utils
import RuntimeSystem (nativeRuntimeSymbols, arduinoRuntimeSymbols)
import Typecheck.Syntax
import Codegen.Utils
import Codegen.CodegenFunction



--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

newModule :: String -> String -> Maybe String -> LLVM.Module
newModule name sourceName targetTriple = LLVM.defaultModule {
    LLVM.moduleName = B.toShort $ BC.pack name,
    LLVM.moduleSourceFileName = B.toShort $ BC.pack sourceName,
    LLVM.moduleDataLayout = Just $ LLVM.defaultDataLayout LLVM.LittleEndian,
    LLVM.moduleTargetTriple = (B.toShort . BC.pack) <$> targetTriple
  }


codegen :: MonadError Error m => Target -> LLVM.Module -> [GlobalDefinition] -> m LLVM.Module
codegen target astModule definitions = evalStateT' (Codegen astModule M.empty) $ do

  case target of
    NativeTarget -> mapM_ importGlobalSymbol $ M.toList nativeRuntimeSymbols
    ArduinoTarget _ _ -> mapM_ importGlobalSymbol $ M.toList arduinoRuntimeSymbols
    _ -> return ()

  mapM_ importGlobalDefinition definitions
  
  case target of
    NativeTarget -> mapM_ codegenExternalGlobalSymbol $ M.toList nativeRuntimeSymbols
    ArduinoTarget _ _ -> mapM_ codegenExternalGlobalSymbol $ M.toList arduinoRuntimeSymbols
    _ -> return ()
   
  mapM_ codegenGlobalDefinition definitions

  gets codegen_module


importGlobalSymbol :: (MonadState Codegen m, MonadError Error m) => (GlobalSymbol, Type) -> m ()
importGlobalSymbol (symbol, type_) = do
  let symbol' = SymbolGlobal $ symbol
  operand <- constantOperand symbol' type_
  addToSymbolTable symbol' $ SymbolProperties type_ operand False


importGlobalDefinition :: (MonadState Codegen m, MonadError Error m) => GlobalDefinition -> m ()
importGlobalDefinition (GlobalDefinitionValue definition) = do
  let symbol = SymbolGlobal $ globalValueDefinition_symbol definition
  let type_ = globalValueDefinition_type definition
  operand <- constantPointerOperand symbol type_
  addToSymbolTable symbol $ SymbolProperties type_ operand True

importGlobalDefinition (GlobalDefinitionFunction definition) = do
  let symbol = SymbolGlobal $ functionDefinition_symbol definition
  let type_ = TypeFunction (map parameter_type $ functionDefinition_parameters definition) (functionDefinition_resultType definition)
  operand <- constantOperand symbol type_
  addToSymbolTable symbol $ SymbolProperties type_ operand False


codegenExternalGlobalSymbol :: (MonadState Codegen m, MonadError Error m) => (GlobalSymbol, Type) -> m ()
codegenExternalGlobalSymbol (symbol, TypeFunction parameterTypes resultType) = do
  let namedParameters = map (\(parameterType, i) -> Parameter ("x" ++ show i) parameterType) $ zipWithIndex parameterTypes
  addGlobalFunction symbol namedParameters resultType []

codegenExternalGlobalSymbol (_, type_) =
  throwError ("(Codegen) codegenLibraryBuiltin not implemented for type " ++ show type_, Nothing)


codegenGlobalDefinition :: (MonadState Codegen m, MonadError Error m) => GlobalDefinition -> m ()
codegenGlobalDefinition (GlobalDefinitionValue definition) = do

  llvmType <- typeToLlvmType $ globalValueDefinition_type definition
  llvmConstant <- case globalValueDefinition_expression definition of
    LiteralExpression value _ _ -> return $ codegenLiteral value
    expression -> throwError ("(Codegen) global values with non-literal initialization are not supported yet", Just $ expressionLoc expression)

  let llvmDefinition = LLVM.GlobalDefinition $ LLVM.globalVariableDefaults {
    LLVM.Global.name = LLVM.Name (B.toShort $ BC.pack $ show $ globalValueDefinition_symbol definition),
    LLVM.Global.isConstant = True,
    LLVM.Global.type' = llvmType,
    LLVM.Global.initializer = Just llvmConstant
  }

  addToModuleDefinitions llvmDefinition

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
    LLVM.Global.name        = LLVM.Name (B.toShort $ BC.pack $ show symbol),
    LLVM.Global.parameters  = (parameters', False),
    LLVM.Global.returnType  = resultType',
    LLVM.Global.basicBlocks = basicBlocks
  }

  addToModuleDefinitions llvmDefinition



--------------------------------------------------------------------------------
-- Codegen primitives and data definition
--------------------------------------------------------------------------------

addToModuleDefinitions :: MonadState Codegen m => LLVM.Definition -> m ()
addToModuleDefinitions definition = do
  module_ <- gets codegen_module
  let moduleDefinitions = LLVM.moduleDefinitions module_
  modify $ \s -> s { codegen_module = module_ { LLVM.moduleDefinitions = moduleDefinitions -:+ definition } }


addToSymbolTable :: MonadState Codegen m => Symbol -> SymbolProperties -> m ()
addToSymbolTable symbol symbolProperties = do
  symbolTable <- gets codegen_symbolTable
  modify $ \s -> s { codegen_symbolTable = M.insert symbol symbolProperties symbolTable }


data Codegen = Codegen {
  codegen_module :: LLVM.Module,
  codegen_symbolTable :: SymbolTable
}
