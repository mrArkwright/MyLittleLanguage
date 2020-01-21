module Codegen.Codegen (initModule, codegen) where


import Control.Monad.State
import Control.Monad.Except

import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Short as B (toShort)
import qualified Data.ByteString.Char8 as BC

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Global as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.Float as LLVM
import qualified LLVM.AST.FloatingPointPredicate as LLVM.FloatingPointPredicate
import qualified LLVM.AST.IntegerPredicate as LLVM.IntegerPredicate
import qualified LLVM.AST.CallingConvention as LLVM
import qualified LLVM.AST.Type as LLVM
import LLVM.AST.Instruction ( Named( (:=) ) )

import Misc
import Builtins
import Parse.Syntax (Name, Type(..), Parameter(..))
import Rename.Syntax (Symbol(..), GlobalSymbol(..), LocalSymbol(..))
import Typecheck.Syntax



--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

type SymbolTable = M.Map Symbol Type

data Codegen = Codegen {
  codegen_module :: LLVM.Module,
  codegen_symbolTable :: SymbolTable
}


initModule :: String -> String -> LLVM.Module
initModule name fileName = LLVM.defaultModule {
    LLVM.moduleName = B.toShort $ BC.pack name,
    LLVM.moduleSourceFileName = B.toShort $ BC.pack fileName
  }


codegen :: MonadError Error m => LLVM.Module -> [GlobalDefinition] -> m LLVM.Module
codegen astModule definitions = evalStateT' (Codegen astModule M.empty) $ do

  mapM_ importBuiltin $ builtins ++ libraryBuiltins
  mapM_ importDefinition definitions

  mapM_ codegenLibraryBuiltin libraryBuiltins
  mapM_ codegenGlobalDefinition definitions

  gets codegen_module


importBuiltin :: (MonadState Codegen m, MonadError Error m) => (Name, Type) -> m ()
importBuiltin (name, type_) = do
  symbolTable <- gets codegen_symbolTable
  let symbol = SymbolGlobal $ GlobalSymbol name []
  modify $ \s -> s { codegen_symbolTable = M.insert symbol type_ symbolTable }


importDefinition :: (MonadState Codegen m, MonadError Error m) => GlobalDefinition -> m ()
importDefinition definition = do
  symbolTable <- gets codegen_symbolTable
  let symbol = SymbolGlobal $ globalDefinitionSymbol definition
  let type_ = globalDefinitionType definition
  modify $ \s -> s { codegen_symbolTable = M.insert symbol type_ symbolTable }


codegenLibraryBuiltin :: (MonadState Codegen m, MonadError Error m) => (Name, Type) -> m ()
codegenLibraryBuiltin (name, type_) = case type_ of

  TypeFunction parameterTypes resultType -> do
    let namedParameters = map (\(parameterType, i) -> Parameter ("x" ++ show i) parameterType) $ zipWithIndex parameterTypes
    addGlobalFunction (GlobalSymbol name []) namedParameters resultType []

  _ -> throwError ("(Codegen) codegenLibraryBuiltin not implemented for type" ++ show type_, Nothing)


codegenGlobalDefinition :: (MonadState Codegen m, MonadError Error m) => GlobalDefinition -> m ()
codegenGlobalDefinition (GlobalDefinitionValue definition) = do
  throwError ("(Codegen) codegenDefinition not implemented for global values.", Just $ globalValueDefinition_loc definition)

codegenGlobalDefinition (GlobalDefinitionFunction definition) = do

  symbolTable <- gets codegen_symbolTable

  let symbol = functionDefinition_symbol definition
  let parameters = functionDefinition_parameters definition
  let expression = functionDefinition_expression definition
  let resultType = functionDefinition_resultType definition

  basicBlocks <- codegenFunction symbolTable parameters expression
  addGlobalFunction symbol parameters resultType basicBlocks


addGlobalFunction :: (MonadState Codegen m, MonadError Error m) => GlobalSymbol -> [Parameter] -> Type -> [LLVM.BasicBlock] -> m ()
addGlobalFunction symbol parameters resultType basicBlocks = do

  let parameters' = map (\parameter -> LLVM.Parameter (typeToType $ parameter_type parameter) (LLVM.Name $ B.toShort $ BC.pack $ parameter_name parameter) []) parameters

  let llvmDefinition = LLVM.GlobalDefinition $ LLVM.functionDefaults {
    LLVM.name        = LLVM.Name (B.toShort $ BC.pack $ show symbol),
    LLVM.parameters  = (parameters', False),
    LLVM.returnType  = typeToType resultType,
    LLVM.basicBlocks = basicBlocks
  }

  llvmModule <- gets codegen_module
  let llvmModuleDefinitions = LLVM.moduleDefinitions llvmModule

  modify $ \s -> s { codegen_module = llvmModule { LLVM.moduleDefinitions = llvmModuleDefinitions -:+ llvmDefinition } }



--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

data CodegenFunction = CodegenFunction {
    codegenFunction_symbolTable           :: SymbolTable,
    codegenFunction_currentBasicBlock     :: BasicBlock,
    codegenFunction_basicBlocks           :: [BasicBlock],
    codegenFunction_symbols               :: M.Map String LLVM.Operand,
    codegenFunction_namedInstructionCount :: Word,
    codegenFunction_labels                :: M.Map String Int
  } deriving Show


newCodegenFunction :: SymbolTable -> CodegenFunction
newCodegenFunction symbolTable = CodegenFunction {
    codegenFunction_symbolTable           = symbolTable,
    codegenFunction_currentBasicBlock     = newBasicBlock "entry",
    codegenFunction_basicBlocks           = [],
    codegenFunction_symbols               = M.empty,
    codegenFunction_namedInstructionCount = 0,
    codegenFunction_labels                = M.empty
  }


data BasicBlock = BasicBlock {
    basicBlock_name         :: String,
    basicBlock_instructions :: [LLVM.Named LLVM.Instruction],
    basicBlock_terminator   :: Maybe (LLVM.Named LLVM.Terminator)
  } deriving Show


newBasicBlock :: String -> BasicBlock
newBasicBlock name = BasicBlock {
    basicBlock_name         = name,
    basicBlock_instructions = [],
    basicBlock_terminator   = Nothing
  }


codegenFunction :: MonadError Error m => SymbolTable -> [Parameter] -> Expression -> m [LLVM.BasicBlock]
codegenFunction symbolTable parameters expression = evalStateT' (newCodegenFunction symbolTable) $ do

  mapM_ addParameter parameters
  result <- codegenExpression expression
  returnValue result

  basicBlocks <- gets codegenFunction_basicBlocks
  currentBasicBlock <- gets codegenFunction_currentBasicBlock
  let basicBlocks' = basicBlocks -:+ currentBasicBlock
  modify $ \s -> s { codegenFunction_basicBlocks = basicBlocks' } -- TODO is this neccessary?

  mapM basicBlockToLLVMBasicBlock basicBlocks'


basicBlockToLLVMBasicBlock :: (MonadState CodegenFunction m, MonadError Error m) => BasicBlock -> m LLVM.BasicBlock
basicBlockToLLVMBasicBlock (BasicBlock name instructions terminator) = do
  terminator' <- maybeToError terminator ("(Codegen) Block has no terminator: " ++ (show name), Nothing)
  return $ LLVM.BasicBlock (LLVM.Name $ B.toShort $ BC.pack name) instructions terminator'


addNewBasicBlock :: (MonadState CodegenFunction m, MonadError Error m) => String -> m ()
addNewBasicBlock name = do

  basicBlocks <- gets codegenFunction_basicBlocks
  currentBasicBlock <- gets codegenFunction_currentBasicBlock

  modify $ \s -> s { codegenFunction_basicBlocks = basicBlocks -:+ currentBasicBlock }
  modify $ \s -> s { codegenFunction_currentBasicBlock = newBasicBlock name }


makeUniqueLabel :: (MonadState CodegenFunction m, MonadError Error m) => String -> m String
makeUniqueLabel label = do

  labels' <- gets codegenFunction_labels

  case M.lookup label labels' of

    Nothing    -> do
      modify $ \s -> s { codegenFunction_labels = M.insert label 1 labels' }
      return label

    Just count -> do
      modify $ \s -> s { codegenFunction_labels = M.insert label (count + 1) labels' }
      return $ label ++ show count



--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

codegenExpression :: (MonadState CodegenFunction m, MonadError Error m) => Expression -> m (Maybe LLVM.Operand)
codegenExpression (Unit _ _) = return Nothing

codegenExpression (Int value _ _) = return $ Just $ LLVM.ConstantOperand $ LLVM.Constant.Int 32 value

codegenExpression (Float value _ _) = return $ Just $ LLVM.ConstantOperand $ LLVM.Constant.Float (LLVM.Double value)

codegenExpression (SymbolReference (SymbolLocal symbol) _ loc) = do
  reference <- getLocalReference $ localSymbol_name symbol
  reference' <- maybeToError reference ("(Codegen) reference to unkown symbol: " ++ localSymbol_name symbol, Just loc)
  return $ Just reference'

codegenExpression (SymbolReference (SymbolGlobal symbol) _ loc) = do
  throwError ("(Codegen) codegen not implemented for global symbols (symbol: " ++ show symbol ++ ")", Just loc)

codegenExpression (Call symbol argExprs _ loc) = do

  args <- map fromJust <$> mapM codegenExpression argExprs

  case symbol of

    SymbolGlobal (GlobalSymbol "+" []) -> do
      let [a, b] = args -- TODO proper error
      Just <$> add a b

    SymbolGlobal (GlobalSymbol "-" []) -> do
      let [a, b] = args
      Just <$> sub a b

    SymbolGlobal (GlobalSymbol "<" []) -> do
      let [a, b] = args
      Just <$> icmp LLVM.IntegerPredicate.SLT a b

    SymbolGlobal (GlobalSymbol "+." []) -> do
      let [a, b] = args
      Just <$> fadd a b

    SymbolGlobal (GlobalSymbol "-." []) -> do
      let [a, b] = args
      Just <$> fsub a b

    SymbolGlobal (GlobalSymbol "*." []) -> do
      let [a, b] = args
      Just <$> fmul a b

    SymbolGlobal (GlobalSymbol "/." []) -> do
      let [a, b] = args
      Just <$> fdiv a b

    SymbolGlobal (GlobalSymbol "<." []) -> do
      let [a, b] = args
      Just <$> fcmp LLVM.FloatingPointPredicate.ULT a b

    _  -> do

      symbolTable <- gets codegenFunction_symbolTable

      (parameterTypes, resultType) <- case M.lookup symbol symbolTable of
        Just (TypeFunction parameterTypes' resultType') -> return (parameterTypes', resultType')
        _ -> throwError ("(Codegen) Call to unknown symbol: " ++ show symbol, Just loc)

      let functionType = LLVM.ptr $ LLVM.FunctionType (typeToType resultType) (map typeToType parameterTypes) False
      let (SymbolGlobal symbol') = symbol
      let function = LLVM.ConstantOperand $ LLVM.Constant.GlobalReference functionType (LLVM.Name $ B.toShort $ BC.pack $ show symbol')

      call (resultType /= TypeUnit) (typeToType resultType) function args

codegenExpression (If condition ifTrue ifFalse ifType loc) = do

  thenLabel <- makeUniqueLabel "if.then"
  elseLabel <- makeUniqueLabel "if.else"
  contLabel <- makeUniqueLabel "if.cont"

  conditionResult <- fromJust <$> codegenExpression condition
  condBr conditionResult thenLabel elseLabel

  addNewBasicBlock thenLabel
  trueResult <- codegenExpression ifTrue
  br contLabel

  addNewBasicBlock elseLabel
  falseResult <- codegenExpression ifFalse
  br contLabel

  addNewBasicBlock contLabel

  case (trueResult, falseResult) of

    (Just trueResult', Just falseResult') -> do
      ret <- phi (typeToType ifType) [(trueResult', thenLabel), (falseResult', elseLabel)]
      return $ Just ret

    (Nothing, Nothing) -> return Nothing

    _ -> throwError ("(Codegen) error", Just loc) -- TODO proper error message


codegenExpression (Do statements _ _) = do
  results <- mapM codegenStatement statements
  return $ last results


codegenStatement :: (MonadState CodegenFunction m, MonadError Error m) => Statement -> m (Maybe LLVM.Operand)
codegenStatement (StatementExpression expression _ _) = codegenExpression expression

codegenStatement (StatementDefinition definition _ _) = do

  result <- fromJust <$> codegenExpression (localValueDefinition_expression definition)

  let symbol = localValueDefinition_symbol definition
  let name = localSymbol_name symbol
  modify $ \s -> s { codegenFunction_symbols = M.insert name result (codegenFunction_symbols s) }

  return Nothing


add :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
add a b = addNamedInstruction integer $ LLVM.Add False False a b []


sub :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
sub a b = addNamedInstruction integer $ LLVM.Sub False False a b []


icmp :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.IntegerPredicate.IntegerPredicate -> LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
icmp condition a b = addNamedInstruction integer $ LLVM.ICmp condition a b []


fadd :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
fadd a b = addNamedInstruction double $ LLVM.FAdd LLVM.noFastMathFlags a b []


fsub :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
fsub a b = addNamedInstruction double $ LLVM.FSub LLVM.noFastMathFlags a b []


fmul :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
fmul a b = addNamedInstruction double $ LLVM.FMul LLVM.noFastMathFlags a b []


fdiv :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
fdiv a b = addNamedInstruction double $ LLVM.FDiv LLVM.noFastMathFlags a b []


fcmp :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.FloatingPointPredicate.FloatingPointPredicate -> LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
fcmp condition a b = addNamedInstruction integer $ LLVM.FCmp condition a b []


call :: (MonadState CodegenFunction m, MonadError Error m) => Bool -> LLVM.Type -> LLVM.Operand -> [LLVM.Operand] -> m (Maybe LLVM.Operand)
call named returnType fn args = do
  let callInstruction = LLVM.Call Nothing LLVM.C [] (Right fn) [(arg, []) | arg <- args] [] []
  if named then Just <$> (addNamedInstruction returnType callInstruction) else do
    addUnnamedInstruction callInstruction
    return Nothing

br :: (MonadState CodegenFunction m, MonadError Error m) => String -> m ()
br label = addTerminator $ LLVM.Do $ LLVM.Br (LLVM.Name $ B.toShort $ BC.pack label) []


condBr :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Operand -> String -> String -> m ()
condBr condition trueLabel falseLabel =

  let trueLabel'  = LLVM.Name $ B.toShort $ BC.pack trueLabel in
  let falseLabel' = LLVM.Name $ B.toShort $ BC.pack falseLabel in

  addTerminator $ LLVM.Do $ LLVM.CondBr condition trueLabel' falseLabel' []


phi :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Type -> [(LLVM.Operand, String)] -> m LLVM.Operand
phi resultType incoming =
  let incoming' = map (\(operand, label) -> (operand, LLVM.Name $ B.toShort $ BC.pack label)) incoming in
  addNamedInstruction resultType $ LLVM.Phi resultType incoming' []


returnValue :: (MonadState CodegenFunction m, MonadError Error m) => Maybe LLVM.Operand -> m ()
returnValue val = addTerminator $ LLVM.Do $ LLVM.Ret val []


addNamedInstruction :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Type -> LLVM.Instruction -> m LLVM.Operand
addNamedInstruction instrType instruction = do

  n <- nextRegisterNumber
  let ref = (LLVM.UnName n)

  currentBasicBlock <- gets codegenFunction_currentBasicBlock
  let currentBasicBlock' = currentBasicBlock { basicBlock_instructions = basicBlock_instructions currentBasicBlock -:+ ref := instruction }

  modify $ \s -> s { codegenFunction_currentBasicBlock = currentBasicBlock' }

  return $ LLVM.LocalReference instrType ref


addUnnamedInstruction :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Instruction -> m ()
addUnnamedInstruction instruction = do

  currentBasicBlock <- gets codegenFunction_currentBasicBlock

  let currentBasicBlock' = currentBasicBlock { basicBlock_instructions = basicBlock_instructions currentBasicBlock -:+ LLVM.Do instruction }

  modify $ \s -> s { codegenFunction_currentBasicBlock = currentBasicBlock' }


nextRegisterNumber :: (MonadState CodegenFunction m, MonadError Error m) => m Word
nextRegisterNumber = do

  n <- gets codegenFunction_namedInstructionCount
  let m = n + 1

  modify $ \s -> s { codegenFunction_namedInstructionCount = m }

  return m


addTerminator :: (MonadState CodegenFunction m, MonadError Error m) => LLVM.Named LLVM.Terminator -> m ()
addTerminator trm = do

  currentBasicBlock' <- gets codegenFunction_currentBasicBlock
  let currentBasicBlock'' = currentBasicBlock' { basicBlock_terminator = Just trm }

  modify $ \s -> s { codegenFunction_currentBasicBlock = currentBasicBlock'' }


addParameter :: (MonadState CodegenFunction m, MonadError Error m) => Parameter -> m LLVM.Operand
addParameter parameter = do
  let symbol = SymbolLocal $ LocalSymbol (parameter_name parameter)
  modify $ \s -> s { codegenFunction_symbolTable = M.insert symbol (parameter_type parameter) (codegenFunction_symbolTable s) }
  addLocalReference (parameter_name parameter, parameter_type parameter)



addLocalReference :: (MonadState CodegenFunction m, MonadError Error m) => (Name, Type) -> m LLVM.Operand
addLocalReference (name, argType) = do

  let newSymbol = LLVM.LocalReference (typeToType argType) (LLVM.Name $ B.toShort $ BC.pack name)

  modify $ \s -> s { codegenFunction_symbols = M.insert name newSymbol (codegenFunction_symbols s) }

  return newSymbol


getLocalReference :: (MonadState CodegenFunction m, MonadError Error m) => String -> m (Maybe LLVM.Operand)
getLocalReference name = do
  symbols' <- gets codegenFunction_symbols
  return $ M.lookup name symbols'


typeToType :: Type -> LLVM.Type
typeToType TypeUnit = LLVM.VoidType
typeToType TypeFloat = double
typeToType TypeInt = integer
typeToType (TypeFunction parameterTypes resultType) = LLVM.ptr $ LLVM.FunctionType (typeToType resultType) (map typeToType parameterTypes) False
typeToType t = error $ "type not implemented: " ++ show t


integer :: LLVM.Type
integer = LLVM.IntegerType 32


double :: LLVM.Type
double = LLVM.FloatingPointType LLVM.DoubleFP

