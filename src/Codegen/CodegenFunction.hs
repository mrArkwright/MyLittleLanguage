module Codegen.CodegenFunction (codegenFunction) where

import Control.Monad.State
import Control.Monad.Except

import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Short as B (toShort)
import qualified Data.ByteString.Char8 as BC

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.AddrSpace as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.Float as LLVM
import qualified LLVM.AST.CallingConvention as LLVM
import LLVM.AST.Instruction ( Named( (:=) ) )

import Misc
import Typecheck.Syntax
import Codegen.Lib
import Codegen.Builtins



--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

codegenFunction :: MonadError Error m => SymbolTable -> FunctionDefinition -> m [LLVM.BasicBlock]
codegenFunction symbolTable definition = evalStateT' (newCodegenFunction symbolTable) $ do

  let parameters = functionDefinition_parameters definition
  let expression = functionDefinition_expression definition

  mapM_ addParameter parameters
  result <- codegenExpression expression
  addReturnValue result

  currentBasicBlock <- gets codegenFunction_currentBasicBlock
  addBasicBlock currentBasicBlock

  basicBlocks <- gets codegenFunction_basicBlocks
  mapM basicBlockToLLVMBasicBlock basicBlocks


addParameter :: (MonadState CodegenFunction m, MonadError Error m) => Parameter -> m ()
addParameter parameter = do

  let name = parameter_name parameter
  let symbol = SymbolLocal $ LocalSymbol name
  let type_ = parameter_type parameter

  type_' <- typeToLlvmType type_

  let operand = LLVM.LocalReference type_' (LLVM.Name $ B.toShort $ BC.pack name)

  addToLocalSymbolTable symbol type_ operand



--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

codegenExpression :: (MonadState CodegenFunction m, MonadError Error m) => Expression -> m (Maybe LLVM.Operand)
codegenExpression (Unit _ _) = return Nothing

codegenExpression (Pointer value _ _) = return $ Just $ LLVM.ConstantOperand $ LLVM.Constant.IntToPtr (LLVM.Constant.Int 32 value) (LLVM.PointerType (LLVM.IntegerType 32) (LLVM.AddrSpace 0))

codegenExpression (Int value _ _) = return $ Just $ LLVM.ConstantOperand $ LLVM.Constant.Int 32 value

codegenExpression (Int8 value _ _) = return $ Just $ LLVM.ConstantOperand $ LLVM.Constant.Int 8 value

codegenExpression (Float value _ _) = return $ Just $ LLVM.ConstantOperand $ LLVM.Constant.Float (LLVM.Double value)

codegenExpression (SymbolReference symbol _ loc) = do
  resolvedSymbol <- resolveSymbol symbol
  (_, operand) <- maybeToError resolvedSymbol ("(Codegen) reference to unkown symbol: " ++ show symbol, Just loc)
  return $ Just operand

codegenExpression (Call symbol argumentExpressions _ loc) = do

  arguments <- map fromJust <$> mapM codegenExpression argumentExpressions

  let builtin = case symbol of
        SymbolGlobal symbol' -> M.lookup symbol' builtins
        _ -> Nothing

  case builtin of

    Just (TypeFunction _ resultType, builtin') -> do

      (argument1, argument2) <- case arguments of
        [argument1', argument2'] -> return (argument1', argument2')
        _ -> throwError ("(Codegen) Wrong number of arguments for builtin " ++ show symbol, Just loc)

      if (resultType /= TypeUnit) then do
        resultType' <- typeToLlvmType resultType
        Just <$> (addNamedInstruction resultType' $ builtin' argument1 argument2)
      else do
        addUnnamedInstruction $ builtin' argument1 argument2
        return Nothing

    Just _ ->
      throwError ("(Codegen) Call to non-function builtin: " ++ show symbol, Just loc)

    Nothing -> do

      symbolTable <- gets codegenFunction_symbolTable

      (resultType, function) <- case M.lookup symbol symbolTable of
        Just (TypeFunction _ resultType', operand') -> return (resultType', operand')
        Just _ -> throwError ("(Codegen) Call to non-function symbol: " ++ show symbol, Just loc)
        _ -> throwError ("(Codegen) Call to unknown symbol: " ++ show symbol, Just loc)

      resultType' <- typeToLlvmType resultType

      addCall (resultType /= TypeUnit) resultType' function arguments

codegenExpression (If condition ifTrue ifFalse ifType loc) = do

  thenLabel <- makeUniqueLabel "if.then"
  elseLabel <- makeUniqueLabel "if.else"
  contLabel <- makeUniqueLabel "if.cont"

  conditionResult <- fromJust <$> codegenExpression condition
  addCondBr conditionResult thenLabel elseLabel

  addNewBasicBlock thenLabel
  trueResult <- codegenExpression ifTrue
  addBr contLabel

  addNewBasicBlock elseLabel
  falseResult <- codegenExpression ifFalse
  addBr contLabel

  addNewBasicBlock contLabel

  case (trueResult, falseResult) of

    (Just trueResult', Just falseResult') -> do
      ifType' <- typeToLlvmType ifType
      ret <- addPhi ifType' [(trueResult', thenLabel), (falseResult', elseLabel)]
      return $ Just ret

    (Nothing, Nothing) -> return Nothing

    _ -> throwError ("(Codegen) branches of if construct don't match", Just loc)


codegenExpression (Do statements _ _) = do
  results <- mapM codegenStatement statements
  return $ last results


codegenStatement :: (MonadState CodegenFunction m, MonadError Error m) => Statement -> m (Maybe LLVM.Operand)
codegenStatement (StatementExpression expression _ _) = codegenExpression expression

codegenStatement (StatementDefinition definition _ _) = do

  result <- fromJust <$> codegenExpression (localValueDefinition_expression definition)

  let symbol = SymbolLocal $ localValueDefinition_symbol definition
  let type_ = localValueDefinition_type definition
  addToLocalSymbolTable symbol type_ result

  return Nothing



--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

addCall :: (MonadState CodegenFunction m, MonadError Error m) => Bool -> LLVM.Type -> LLVM.Operand -> [LLVM.Operand] -> m (Maybe LLVM.Operand)
addCall named returnType fn args = do

  let callInstruction = LLVM.Call Nothing LLVM.C [] (Right fn) [(arg, []) | arg <- args] [] []

  if named then
    Just <$> addNamedInstruction returnType callInstruction
  else do
    addUnnamedInstruction callInstruction
    return Nothing


addBr :: (MonadState CodegenFunction m, MonadError Error m) => String -> m ()
addBr label = addTerminator $ LLVM.Do $ LLVM.Br (LLVM.Name $ B.toShort $ BC.pack label) []


addCondBr :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Operand -> String -> String -> m ()
addCondBr condition trueLabel falseLabel =

  let trueLabel'  = LLVM.Name $ B.toShort $ BC.pack trueLabel in
  let falseLabel' = LLVM.Name $ B.toShort $ BC.pack falseLabel in

  addTerminator $ LLVM.Do $ LLVM.CondBr condition trueLabel' falseLabel' []


addPhi :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Type -> [(LLVM.Operand, String)] -> m LLVM.Operand
addPhi resultType incoming =
  let incoming' = map (\(operand, label) -> (operand, LLVM.Name $ B.toShort $ BC.pack label)) incoming in
  addNamedInstruction resultType $ LLVM.Phi resultType incoming' []


addNamedInstruction :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Type -> LLVM.Instruction -> m LLVM.Operand
addNamedInstruction instrType instruction = do

  n <- nextRegisterNumber
  let ref = LLVM.UnName n

  addInstruction $ ref := instruction

  return $ LLVM.LocalReference instrType ref


addUnnamedInstruction :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Instruction -> m ()
addUnnamedInstruction instruction = addInstruction $ LLVM.Do instruction


addReturnValue :: (MonadState CodegenFunction m, MonadError Error m) => Maybe LLVM.Operand -> m ()
addReturnValue val = addTerminator $ LLVM.Do $ LLVM.Ret val []



--------------------------------------------------------------------------------
-- CodegenFunction primitives and data definition
--------------------------------------------------------------------------------

addToLocalSymbolTable :: (MonadState CodegenFunction m, MonadError Error m) => Symbol -> Type -> LLVM.Operand -> m ()
addToLocalSymbolTable symbol type_ operand = do
  symbolTable <- gets codegenFunction_symbolTable
  modify $ \s -> s { codegenFunction_symbolTable = M.insert symbol (type_, operand) symbolTable }


resolveSymbol :: (MonadState CodegenFunction m, MonadError Error m) => Symbol -> m (Maybe (Type, LLVM.Operand))
resolveSymbol name = do
  symbolTable <- gets codegenFunction_symbolTable
  return $ M.lookup name symbolTable


addNewBasicBlock :: (MonadState CodegenFunction m, MonadError Error m) => String -> m ()
addNewBasicBlock name = do
  currentBasicBlock <- gets codegenFunction_currentBasicBlock
  addBasicBlock currentBasicBlock
  modify $ \s -> s { codegenFunction_currentBasicBlock = newBasicBlock name }


addBasicBlock :: (MonadState CodegenFunction m, MonadError Error m) => BasicBlock -> m ()
addBasicBlock basicBlock = do
  basicBlocks <- gets codegenFunction_basicBlocks
  modify $ \s -> s { codegenFunction_basicBlocks = basicBlocks -:+ basicBlock }


addInstruction :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Named LLVM.Instruction -> m ()
addInstruction instruction = do
  currentBasicBlock <- gets codegenFunction_currentBasicBlock
  let currentBasicBlock' = currentBasicBlock { basicBlock_instructions = basicBlock_instructions currentBasicBlock -:+ instruction }
  modify $ \s -> s { codegenFunction_currentBasicBlock = currentBasicBlock' }


addTerminator :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Named LLVM.Terminator -> m ()
addTerminator trm = do
  currentBasicBlock <- gets codegenFunction_currentBasicBlock
  let currentBasicBlock' = currentBasicBlock { basicBlock_terminator = Just trm }
  modify $ \s -> s { codegenFunction_currentBasicBlock = currentBasicBlock' }


nextRegisterNumber :: (MonadState CodegenFunction m, MonadError Error m) => m Word
nextRegisterNumber = do

  n <- gets codegenFunction_namedInstructionCount
  let m = n + 1

  modify $ \s -> s { codegenFunction_namedInstructionCount = m }

  return m


makeUniqueLabel :: (MonadState CodegenFunction m, MonadError Error m) => String -> m String
makeUniqueLabel label = do

  labels' <- gets codegenFunction_labels

  case M.lookup label labels' of

    Nothing -> do
      modify $ \s -> s { codegenFunction_labels = M.insert label 1 labels' }
      return label

    Just count -> do
      modify $ \s -> s { codegenFunction_labels = M.insert label (count + 1) labels' }
      return $ label ++ show count


newCodegenFunction :: SymbolTable -> CodegenFunction
newCodegenFunction symbolTable = CodegenFunction {
    codegenFunction_symbolTable = symbolTable,
    codegenFunction_currentBasicBlock = newBasicBlock "entry",
    codegenFunction_basicBlocks = [],
    codegenFunction_namedInstructionCount = 0,
    codegenFunction_labels = M.empty
  }

data CodegenFunction = CodegenFunction {
    codegenFunction_symbolTable :: SymbolTable,
    codegenFunction_currentBasicBlock :: BasicBlock,
    codegenFunction_basicBlocks :: [BasicBlock],
    codegenFunction_namedInstructionCount :: Word,
    codegenFunction_labels :: M.Map String Int
  } deriving Show



--------------------------------------------------------------------------------
-- BasicBlock primitives and data definition
--------------------------------------------------------------------------------

basicBlockToLLVMBasicBlock :: MonadError Error m => BasicBlock -> m LLVM.BasicBlock
basicBlockToLLVMBasicBlock (BasicBlock name instructions terminator) = do
  terminator' <- maybeToError terminator ("(Codegen) Block has no terminator: " ++ (show name), Nothing)
  return $ LLVM.BasicBlock (LLVM.Name $ B.toShort $ BC.pack name) instructions terminator'


newBasicBlock :: String -> BasicBlock
newBasicBlock name = BasicBlock {
    basicBlock_name = name,
    basicBlock_instructions = [],
    basicBlock_terminator = Nothing
  }

data BasicBlock = BasicBlock {
    basicBlock_name :: String,
    basicBlock_instructions :: [LLVM.Named LLVM.Instruction],
    basicBlock_terminator :: Maybe (LLVM.Named LLVM.Terminator)
  } deriving Show
