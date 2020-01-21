module Codegen.Codegen (initModule, codegen) where


import Control.Monad.State
import Control.Monad.Except

import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Short as B (toShort)
import qualified Data.ByteString.Char8 as BC

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as AST
import qualified LLVM.AST.Constant as AST.C
import qualified LLVM.AST.Float as AST
import qualified LLVM.AST.FloatingPointPredicate as FPred
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.CallingConvention as AST
import qualified LLVM.AST.Type as AST
import LLVM.AST.Instruction ( Named( (:=) ) )

import Misc
import Builtins
import qualified Parse.Syntax as S (Name, Type(..), Parameter(..))
import qualified Rename.Syntax as S (Symbol(..), GlobalSymbol(..), LocalSymbol(..))
import qualified Typecheck.Syntax as S



--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

type SymbolTable = M.Map S.Symbol S.Type

data Codegen = Codegen {
  codegen_module :: AST.Module,
  codegen_symbolTable :: SymbolTable
}


initModule :: String -> String -> AST.Module
initModule name fileName = AST.defaultModule {
    AST.moduleName = B.toShort $ BC.pack name,
    AST.moduleSourceFileName = B.toShort $ BC.pack fileName
  }


codegen :: MonadError Error m => AST.Module -> [S.GlobalDefinition] -> m AST.Module
codegen astModule definitions = evalStateT' (Codegen astModule M.empty) $ do

  mapM_ importBuiltin $ builtins ++ libraryBuiltins
  mapM_ importDefinition definitions

  mapM_ codegenLibraryBuiltin libraryBuiltins
  mapM_ codegenGlobalDefinition definitions

  gets codegen_module


importBuiltin :: (MonadState Codegen m, MonadError Error m) => (S.Name, S.Type) -> m ()
importBuiltin (name, type_) = do
  symbolTable <- gets codegen_symbolTable
  let symbol = S.SymbolGlobal $ S.GlobalSymbol name []
  modify $ \s -> s { codegen_symbolTable = M.insert symbol type_ symbolTable }


importDefinition :: (MonadState Codegen m, MonadError Error m) => S.GlobalDefinition -> m ()
importDefinition definition = do
  symbolTable <- gets codegen_symbolTable
  let symbol = S.SymbolGlobal $ S.globalDefinitionSymbol definition
  let type_ = S.globalDefinitionType definition
  modify $ \s -> s { codegen_symbolTable = M.insert symbol type_ symbolTable }


codegenLibraryBuiltin :: (MonadState Codegen m, MonadError Error m) => (S.Name, S.Type) -> m ()
codegenLibraryBuiltin (name, type_) = case type_ of

  S.TypeFunction parameterTypes resultType -> do
    let namedParameters = map (\(parameterType, i) -> S.Parameter ("x" ++ show i) parameterType) $ zipWithIndex parameterTypes
    addGlobalFunction (S.GlobalSymbol name []) namedParameters resultType []

  _ -> throwError ("(Codegen) codegenLibraryBuiltin not implemented for type" ++ show type_, Nothing)


codegenGlobalDefinition :: (MonadState Codegen m, MonadError Error m) => S.GlobalDefinition -> m ()
codegenGlobalDefinition (S.GlobalDefinitionValue definition) = do
  throwError ("(Codegen) codegenDefinition not implemented for global values.", Just $ S.globalValueDefinition_loc definition)

codegenGlobalDefinition (S.GlobalDefinitionFunction definition) = do

  symbolTable <- gets codegen_symbolTable

  let symbol = S.functionDefinition_symbol definition
  let parameters = S.functionDefinition_parameters definition
  let expression = S.functionDefinition_expression definition
  let resultType = S.functionDefinition_resultType definition

  basicBlocks <- codegenFunction symbolTable parameters expression
  addGlobalFunction symbol parameters resultType basicBlocks


addGlobalFunction :: (MonadState Codegen m, MonadError Error m) => S.GlobalSymbol -> [S.Parameter] -> S.Type -> [AST.BasicBlock] -> m ()
addGlobalFunction symbol parameters resultType basicBlocks = do

  let parameters' = map (\parameter -> AST.Parameter (typeToType $ S.parameter_type parameter) (AST.Name $ B.toShort $ BC.pack $ S.parameter_name parameter) []) parameters

  let llvmDefinition = AST.GlobalDefinition $ AST.functionDefaults {
    AST.name        = AST.Name (B.toShort $ BC.pack $ show symbol),
    AST.parameters  = (parameters', False),
    AST.returnType  = typeToType resultType,
    AST.basicBlocks = basicBlocks
  }

  llvmModule <- gets codegen_module
  let llvmModuleDefinitions = AST.moduleDefinitions llvmModule

  modify $ \s -> s { codegen_module = llvmModule { AST.moduleDefinitions = llvmModuleDefinitions -:+ llvmDefinition } }



--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

data CodegenFunction = CodegenFunction {
    codegenFunction_symbolTable           :: SymbolTable,
    codegenFunction_currentBasicBlock     :: BasicBlock,
    codegenFunction_basicBlocks           :: [BasicBlock],
    codegenFunction_symbols               :: M.Map String AST.Operand,
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
    basicBlock_instructions :: [AST.Named AST.Instruction],
    basicBlock_terminator   :: Maybe (AST.Named AST.Terminator)
  } deriving Show


newBasicBlock :: String -> BasicBlock
newBasicBlock name = BasicBlock {
    basicBlock_name         = name,
    basicBlock_instructions = [],
    basicBlock_terminator   = Nothing
  }


codegenFunction :: MonadError Error m => SymbolTable -> [S.Parameter] -> S.Expression -> m [AST.BasicBlock]
codegenFunction symbolTable parameters expression = evalStateT' (newCodegenFunction symbolTable) $ do

  mapM_ addParameter parameters
  result <- codegenExpression expression
  returnValue result

  basicBlocks <- gets codegenFunction_basicBlocks
  currentBasicBlock <- gets codegenFunction_currentBasicBlock
  let basicBlocks' = basicBlocks -:+ currentBasicBlock
  modify $ \s -> s { codegenFunction_basicBlocks = basicBlocks' } -- TODO is this neccessary?

  mapM basicBlockToLLVMBasicBlock basicBlocks'


basicBlockToLLVMBasicBlock :: (MonadState CodegenFunction m, MonadError Error m) => BasicBlock -> m AST.BasicBlock
basicBlockToLLVMBasicBlock (BasicBlock name instructions terminator) = do
  terminator' <- maybeToError terminator ("(Codegen) Block has no terminator: " ++ (show name), Nothing)
  return $ AST.BasicBlock (AST.Name $ B.toShort $ BC.pack name) instructions terminator'


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

codegenExpression :: (MonadState CodegenFunction m, MonadError Error m) => S.Expression -> m (Maybe AST.Operand)
codegenExpression (S.Unit _ _) = return Nothing

codegenExpression (S.Int value _ _) = return $ Just $ AST.ConstantOperand $ AST.C.Int 32 value

codegenExpression (S.Float value _ _) = return $ Just $ AST.ConstantOperand $ AST.C.Float (AST.Double value)

codegenExpression (S.SymbolReference (S.SymbolLocal symbol) _ loc) = do
  reference <- getLocalReference $ S.localSymbol_name symbol
  reference' <- maybeToError reference ("(Codegen) reference to unkown symbol: " ++ S.localSymbol_name symbol, Just loc)
  return $ Just reference'

codegenExpression (S.SymbolReference (S.SymbolGlobal symbol) _ loc) = do
  throwError ("(Codegen) codegen not implemented for global symbols (symbol: " ++ show symbol ++ ")", Just loc)

codegenExpression (S.Call symbol argExprs _ loc) = do

  args <- map fromJust <$> mapM codegenExpression argExprs

  case symbol of

    S.SymbolGlobal (S.GlobalSymbol "+" []) -> do
      let [a, b] = args -- TODO proper error
      Just <$> add a b

    S.SymbolGlobal (S.GlobalSymbol "-" []) -> do
      let [a, b] = args
      Just <$> sub a b

    S.SymbolGlobal (S.GlobalSymbol "<" []) -> do
      let [a, b] = args
      Just <$> icmp IPred.SLT a b

    S.SymbolGlobal (S.GlobalSymbol "+." []) -> do
      let [a, b] = args
      Just <$> fadd a b

    S.SymbolGlobal (S.GlobalSymbol "-." []) -> do
      let [a, b] = args
      Just <$> fsub a b

    S.SymbolGlobal (S.GlobalSymbol "*." []) -> do
      let [a, b] = args
      Just <$> fmul a b

    S.SymbolGlobal (S.GlobalSymbol "/." []) -> do
      let [a, b] = args
      Just <$> fdiv a b

    S.SymbolGlobal (S.GlobalSymbol "<." []) -> do
      let [a, b] = args
      Just <$> fcmp FPred.ULT a b

    _  -> do

      symbolTable <- gets codegenFunction_symbolTable

      (parameterTypes, resultType) <- case M.lookup symbol symbolTable of
        Just (S.TypeFunction parameterTypes' resultType') -> return (parameterTypes', resultType')
        _ -> throwError ("(Codegen) Call to unknown symbol: " ++ show symbol, Just loc)

      let functionType = AST.ptr $ AST.FunctionType (typeToType resultType) (map typeToType parameterTypes) False
      let (S.SymbolGlobal symbol') = symbol
      let function = AST.ConstantOperand $ AST.C.GlobalReference functionType (AST.Name $ B.toShort $ BC.pack $ show symbol')

      call (resultType /= S.TypeUnit) (typeToType resultType) function args

codegenExpression (S.If condition ifTrue ifFalse ifType loc) = do

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


codegenExpression (S.Do statements _ _) = do
  results <- mapM codegenStatement statements
  return $ last results


codegenStatement :: (MonadState CodegenFunction m, MonadError Error m) => S.Statement -> m (Maybe AST.Operand)
codegenStatement (S.StatementExpression expression _ _) = codegenExpression expression

codegenStatement (S.StatementDefinition definition _ _) = do

  result <- fromJust <$> codegenExpression (S.localValueDefinition_expression definition)

  let symbol = S.localValueDefinition_symbol definition
  let name = S.localSymbol_name symbol
  modify $ \s -> s { codegenFunction_symbols = M.insert name result (codegenFunction_symbols s) }

  return Nothing


add :: (MonadState CodegenFunction m, MonadError Error m) => AST.Operand -> AST.Operand -> m AST.Operand
add a b = addNamedInstruction integer $ AST.Add False False a b []


sub :: (MonadState CodegenFunction m, MonadError Error m) => AST.Operand -> AST.Operand -> m AST.Operand
sub a b = addNamedInstruction integer $ AST.Sub False False a b []


icmp :: (MonadState CodegenFunction m, MonadError Error m) => IPred.IntegerPredicate -> AST.Operand -> AST.Operand -> m AST.Operand
icmp condition a b = addNamedInstruction integer $ AST.ICmp condition a b []


fadd :: (MonadState CodegenFunction m, MonadError Error m) => AST.Operand -> AST.Operand -> m AST.Operand
fadd a b = addNamedInstruction double $ AST.FAdd AST.noFastMathFlags a b []


fsub :: (MonadState CodegenFunction m, MonadError Error m) => AST.Operand -> AST.Operand -> m AST.Operand
fsub a b = addNamedInstruction double $ AST.FSub AST.noFastMathFlags a b []


fmul :: (MonadState CodegenFunction m, MonadError Error m) => AST.Operand -> AST.Operand -> m AST.Operand
fmul a b = addNamedInstruction double $ AST.FMul AST.noFastMathFlags a b []


fdiv :: (MonadState CodegenFunction m, MonadError Error m) => AST.Operand -> AST.Operand -> m AST.Operand
fdiv a b = addNamedInstruction double $ AST.FDiv AST.noFastMathFlags a b []


fcmp :: (MonadState CodegenFunction m, MonadError Error m) => FPred.FloatingPointPredicate -> AST.Operand -> AST.Operand -> m AST.Operand
fcmp condition a b = addNamedInstruction integer $ AST.FCmp condition a b []


call :: (MonadState CodegenFunction m, MonadError Error m) => Bool -> AST.Type -> AST.Operand -> [AST.Operand] -> m (Maybe AST.Operand)
call named returnType fn args = do
  let callInstruction = AST.Call Nothing AST.C [] (Right fn) [(arg, []) | arg <- args] [] []
  if named then Just <$> (addNamedInstruction returnType callInstruction) else do
    addUnnamedInstruction callInstruction
    return Nothing

br :: (MonadState CodegenFunction m, MonadError Error m) => String -> m ()
br label = addTerminator $ AST.Do $ AST.Br (AST.Name $ B.toShort $ BC.pack label) []


condBr :: (MonadState CodegenFunction m, MonadError Error m) => AST.Operand -> String -> String -> m ()
condBr condition trueLabel falseLabel =

  let trueLabel'  = AST.Name $ B.toShort $ BC.pack trueLabel in
  let falseLabel' = AST.Name $ B.toShort $ BC.pack falseLabel in

  addTerminator $ AST.Do $ AST.CondBr condition trueLabel' falseLabel' []


phi :: (MonadState CodegenFunction m, MonadError Error m) => AST.Type -> [(AST.Operand, String)] -> m AST.Operand
phi resultType incoming =
  let incoming' = map (\(operand, label) -> (operand, AST.Name $ B.toShort $ BC.pack label)) incoming in
  addNamedInstruction resultType $ AST.Phi resultType incoming' []


returnValue :: (MonadState CodegenFunction m, MonadError Error m) => Maybe AST.Operand -> m ()
returnValue val = addTerminator $ AST.Do $ AST.Ret val []


addNamedInstruction :: (MonadState CodegenFunction m, MonadError Error m) => AST.Type -> AST.Instruction -> m AST.Operand
addNamedInstruction instrType instruction = do

  n <- nextRegisterNumber
  let ref = (AST.UnName n)

  currentBasicBlock <- gets codegenFunction_currentBasicBlock
  let currentBasicBlock' = currentBasicBlock { basicBlock_instructions = basicBlock_instructions currentBasicBlock -:+ ref := instruction }

  modify $ \s -> s { codegenFunction_currentBasicBlock = currentBasicBlock' }

  return $ AST.LocalReference instrType ref


addUnnamedInstruction :: (MonadState CodegenFunction m, MonadError Error m) => AST.Instruction -> m ()
addUnnamedInstruction instruction = do

  currentBasicBlock <- gets codegenFunction_currentBasicBlock

  let currentBasicBlock' = currentBasicBlock { basicBlock_instructions = basicBlock_instructions currentBasicBlock -:+ AST.Do instruction }

  modify $ \s -> s { codegenFunction_currentBasicBlock = currentBasicBlock' }


nextRegisterNumber :: (MonadState CodegenFunction m, MonadError Error m) => m Word
nextRegisterNumber = do

  n <- gets codegenFunction_namedInstructionCount
  let m = n + 1

  modify $ \s -> s { codegenFunction_namedInstructionCount = m }

  return m


addTerminator :: (MonadState CodegenFunction m, MonadError Error m) => AST.Named AST.Terminator -> m ()
addTerminator trm = do

  currentBasicBlock' <- gets codegenFunction_currentBasicBlock
  let currentBasicBlock'' = currentBasicBlock' { basicBlock_terminator = Just trm }

  modify $ \s -> s { codegenFunction_currentBasicBlock = currentBasicBlock'' }


addParameter :: (MonadState CodegenFunction m, MonadError Error m) => S.Parameter -> m AST.Operand
addParameter parameter = do
  let symbol = S.SymbolLocal $ S.LocalSymbol (S.parameter_name parameter)
  modify $ \s -> s { codegenFunction_symbolTable = M.insert symbol (S.parameter_type parameter) (codegenFunction_symbolTable s) }
  addLocalReference (S.parameter_name parameter, S.parameter_type parameter)



addLocalReference :: (MonadState CodegenFunction m, MonadError Error m) => (S.Name, S.Type) -> m AST.Operand
addLocalReference (name, argType) = do

  let newSymbol = AST.LocalReference (typeToType argType) (AST.Name $ B.toShort $ BC.pack name)

  modify $ \s -> s { codegenFunction_symbols = M.insert name newSymbol (codegenFunction_symbols s) }

  return newSymbol


getLocalReference :: (MonadState CodegenFunction m, MonadError Error m) => String -> m (Maybe AST.Operand)
getLocalReference name = do
  symbols' <- gets codegenFunction_symbols
  return $ M.lookup name symbols'


typeToType :: S.Type -> AST.Type
typeToType S.TypeUnit = AST.VoidType
typeToType S.TypeFloat = double
typeToType S.TypeInt = integer
typeToType (S.TypeFunction parameterTypes resultType) = AST.ptr $ AST.FunctionType (typeToType resultType) (map typeToType parameterTypes) False
typeToType t = error $ "type not implemented: " ++ show t


integer :: AST.Type
integer = AST.IntegerType 32


double :: AST.Type
double = AST.FloatingPointType AST.DoubleFP

