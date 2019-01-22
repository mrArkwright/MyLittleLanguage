module Codegen (initModule, codegen) where


import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Morph

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
import qualified Syntax as S



type SymbolTable = M.Map S.Symbol S.FuncSignature



--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

type Codegen = StateT AST.Module (Except Error)


initModule :: String -> String -> AST.Module
initModule name fileName = AST.defaultModule {
    AST.moduleName = B.toShort $ BC.pack name,
    AST.moduleSourceFileName = B.toShort $ BC.pack fileName
  }


codegen :: Monad m => AST.Module -> [(S.Def S.Type)] -> ExceptT Error m AST.Module
codegen astModule definitions = hoist generalize $ execStateT' astModule $ do

  let functionDeclarations = builtins ++ libraryBuiltins ++ map S.defToFuncDecl definitions
  let symbolTable = M.fromList $ map (\(S.FuncDecl symbol signature) -> (symbol, signature)) functionDeclarations

  mapM_ codegenDeclaration libraryBuiltins
  mapM_ (codegenDefinition symbolTable) definitions


codegenDeclaration :: S.FuncDecl -> Codegen ()
codegenDeclaration (S.FuncDecl symbol (S.FuncSignature returnType args)) = addGlobalFunction symbol returnType namedArgs [] where
  namedArgs = map (\(arg, i) -> ("x" ++ show i, arg)) $ zip args [(1 :: Int)..]


codegenDefinition :: SymbolTable -> S.Def S.Type -> Codegen ()
codegenDefinition symbolTable (S.Function symbol returnType args body _) = do
  basicBlocks <- lift $ codegenFunction symbolTable args body
  addGlobalFunction symbol returnType args basicBlocks


addGlobalFunction :: S.Symbol -> S.Type -> [(S.Name, S.Type)] -> [AST.BasicBlock] -> Codegen ()
addGlobalFunction symbol returnType args basicBlocks = do

  let args' = map (\(argName, argType) -> AST.Parameter (typeToType argType) (AST.Name $ B.toShort $ BC.pack $ argName) []) args

  let def = AST.GlobalDefinition $ AST.functionDefaults {
    AST.name        = AST.Name (B.toShort $ BC.pack $ S.symbolToString symbol),
    AST.parameters  = (args', False),
    AST.returnType  = typeToType returnType,
    AST.basicBlocks = basicBlocks
  }

  defs <- gets AST.moduleDefinitions

  modify $ \s -> s { AST.moduleDefinitions = defs -:+ def }



--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

data Function = Function {
    _currentBasicBlock     :: BasicBlock,
    _basicBlocks           :: [BasicBlock],
    _symbols               :: M.Map String AST.Operand,
    _namedInstructionCount :: Word,
    _labels                :: M.Map String Int
  } deriving Show


emptyFunction :: Function
emptyFunction = Function {
    _currentBasicBlock     = emptyBasicBlock "entry",
    _basicBlocks           = [],
    _symbols               = M.empty,
    _namedInstructionCount = 0,
    _labels                = M.empty
  }


data BasicBlock = BasicBlock {
    _name         :: String,
    _instructions :: [AST.Named AST.Instruction],
    _terminator   :: Maybe (AST.Named AST.Terminator)
  } deriving Show


emptyBasicBlock :: String -> BasicBlock
emptyBasicBlock name = BasicBlock {
    _name         = name,
    _instructions = [],
    _terminator   = Nothing
  }


type CodegenFunction = ReaderT SymbolTable (StateT Function (Except Error))


codegenFunction :: SymbolTable -> [(S.Name, S.Type)] -> S.Expr S.Type -> Except Error [AST.BasicBlock]
codegenFunction symbolTable args body = evalStateT' emptyFunction $ runReaderT' symbolTable $ do

  mapM_ addLocalReference args
  result <- codegenExpression body
  returnValue result

  basicBlocks' <- gets _basicBlocks
  currentBasicBlock' <- gets _currentBasicBlock
  modify $ \s -> s { _basicBlocks = basicBlocks' -:+ currentBasicBlock' }

  basicBlocks'' <- gets _basicBlocks
  mapM basicBlockToLLVMBasicBlock basicBlocks''


basicBlockToLLVMBasicBlock :: BasicBlock -> CodegenFunction AST.BasicBlock
basicBlockToLLVMBasicBlock (BasicBlock name' instructions' terminator') = do
  terminator'' <- maybeToExcept terminator' ("Block has no terminator: " ++ (show name'), Nothing)
  return $ AST.BasicBlock (AST.Name $ B.toShort $ BC.pack name') instructions' terminator''


newBasicBlock :: String -> CodegenFunction ()
newBasicBlock name = do

  basicBlocks' <- gets _basicBlocks

  currentBasicBlock' <- gets _currentBasicBlock

  modify $ \s -> s { _basicBlocks = basicBlocks' -:+ currentBasicBlock' }
  modify $ \s -> s { _currentBasicBlock = emptyBasicBlock name }


makeUniqueLabel :: String -> CodegenFunction String
makeUniqueLabel label = do

  labels' <- gets _labels

  case M.lookup label labels' of

    Nothing    -> do
      modify $ \s -> s { _labels = M.insert label 1 labels' }
      return label

    Just count -> do
      modify $ \s -> s { _labels = M.insert label (count + 1) labels' }
      return $ label ++ show count



--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

codegenExpression :: S.Expr S.Type -> CodegenFunction (Maybe AST.Operand)
codegenExpression (S.Unit _ _) = return Nothing

codegenExpression (S.Int value _ _) = return $ Just $ AST.ConstantOperand $ AST.C.Int 32 value

codegenExpression (S.Float value _ _) = return $ Just $ AST.ConstantOperand $ AST.C.Float (AST.Double value)

codegenExpression (S.Var name _ _) = do
  reference <- getLocalReference name
  return $ Just $ maybeError reference ("no such symbol: " ++ name)

codegenExpression (S.If condition ifTrue ifFalse ifType loc) = do

  thenLabel <- makeUniqueLabel "if.then"
  elseLabel <- makeUniqueLabel "if.else"
  contLabel <- makeUniqueLabel "if.cont"

  conditionResult <- fromJust <$> codegenExpression condition
  condBr conditionResult thenLabel elseLabel

  newBasicBlock thenLabel
  trueResult <- codegenExpression ifTrue
  br contLabel

  newBasicBlock elseLabel
  falseResult <- codegenExpression ifFalse
  br contLabel

  newBasicBlock contLabel

  case (trueResult, falseResult) of

    (Just trueResult', Just falseResult') -> do
      ret <- phi (typeToType ifType) [(trueResult', thenLabel), (falseResult', elseLabel)]
      return $ Just ret

    (Nothing, Nothing) -> return Nothing

    _ -> throwError ("error", Just loc) -- TODO proper error message

codegenExpression (S.Call symbol argExprs _ _) = do

  args <- map fromJust <$> mapM codegenExpression argExprs

  case symbol of

    S.Symbol "+" [] -> do
      let [a, b] = args -- TODO proper error
      Just <$> add a b

    S.Symbol "-" [] -> do
      let [a, b] = args
      Just <$> sub a b

    S.Symbol "<" [] -> do
      let [a, b] = args
      Just <$> icmp IPred.SLT a b

    S.Symbol "+." [] -> do
      let [a, b] = args
      Just <$> fadd a b

    S.Symbol "-." [] -> do
      let [a, b] = args
      Just <$> fsub a b

    S.Symbol "*." [] -> do
      let [a, b] = args
      Just <$> fmul a b

    S.Symbol "/." [] -> do
      let [a, b] = args
      Just <$> fdiv a b

    S.Symbol "<." [] -> do
      let [a, b] = args
      Just <$> fcmp FPred.ULT a b

    _  -> do

      symbolTable <- ask

      let S.FuncSignature returnType argTypes = fromJust $ M.lookup symbol symbolTable
      let functionType = AST.ptr $ AST.FunctionType (typeToType returnType) (map typeToType argTypes) False
      let function = AST.ConstantOperand $ AST.C.GlobalReference functionType (AST.Name $ B.toShort $ BC.pack $ S.symbolToString symbol)

      call (returnType /= S.TypeUnit) (typeToType returnType) function args

codegenExpression (S.Do statements _ _) = do
  results <- mapM codegenStatement statements
  return $ last results


codegenStatement :: S.Statement S.Type -> CodegenFunction (Maybe AST.Operand)
codegenStatement (S.Expr expression _ _) = codegenExpression expression

codegenStatement (S.Let name _ expression _ _) = do

  result <- fromJust <$> codegenExpression expression

  modify $ \s -> s { _symbols = M.insert name result (_symbols s) }

  return Nothing


add :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
add a b = addNamedInstruction integer $ AST.Add False False a b []


sub :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
sub a b = addNamedInstruction integer $ AST.Sub False False a b []


icmp :: IPred.IntegerPredicate -> AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
icmp condition a b = addNamedInstruction integer $ AST.ICmp condition a b []


fadd :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fadd a b = addNamedInstruction double $ AST.FAdd AST.noFastMathFlags a b []


fsub :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fsub a b = addNamedInstruction double $ AST.FSub AST.noFastMathFlags a b []


fmul :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fmul a b = addNamedInstruction double $ AST.FMul AST.noFastMathFlags a b []


fdiv :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fdiv a b = addNamedInstruction double $ AST.FDiv AST.noFastMathFlags a b []


fcmp :: FPred.FloatingPointPredicate -> AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fcmp condition a b = addNamedInstruction integer $ AST.FCmp condition a b []


call :: Bool -> AST.Type -> AST.Operand -> [AST.Operand] -> CodegenFunction (Maybe AST.Operand)
call named returnType fn args = do
  let callInstruction = AST.Call Nothing AST.C [] (Right fn) [(arg, []) | arg <- args] [] []
  if named then Just <$> (addNamedInstruction returnType callInstruction) else do
    addUnnamedInstruction callInstruction
    return Nothing

br :: String -> CodegenFunction ()
br label = addTerminator $ AST.Do $ AST.Br (AST.Name $ B.toShort $ BC.pack label) []


condBr :: AST.Operand -> String -> String -> CodegenFunction ()
condBr condition trueLabel falseLabel =

  let trueLabel'  = AST.Name $ B.toShort $ BC.pack trueLabel in
  let falseLabel' = AST.Name $ B.toShort $ BC.pack falseLabel in

  addTerminator $ AST.Do $ AST.CondBr condition trueLabel' falseLabel' []


phi :: AST.Type -> [(AST.Operand, String)] -> CodegenFunction AST.Operand
phi resultType incoming =
  let incoming' = map (\(operand, label) -> (operand, AST.Name $ B.toShort $ BC.pack label)) incoming in
  addNamedInstruction resultType $ AST.Phi resultType incoming' []


returnValue :: Maybe AST.Operand -> CodegenFunction ()
returnValue val = addTerminator $ AST.Do $ AST.Ret val []


addNamedInstruction :: AST.Type -> AST.Instruction -> CodegenFunction AST.Operand
addNamedInstruction instrType instruction = do

  n <- nextRegisterNumber
  let ref = (AST.UnName n)

  currentBasicBlock <- gets _currentBasicBlock
  let currentBasicBlock' = currentBasicBlock { _instructions = _instructions currentBasicBlock -:+ ref := instruction }

  modify $ \s -> s { _currentBasicBlock = currentBasicBlock' }

  return $ AST.LocalReference instrType ref


addUnnamedInstruction :: AST.Instruction -> CodegenFunction ()
addUnnamedInstruction instruction = do

  currentBasicBlock <- gets _currentBasicBlock

  let currentBasicBlock' = currentBasicBlock { _instructions = _instructions currentBasicBlock -:+ AST.Do instruction }

  modify $ \s -> s { _currentBasicBlock = currentBasicBlock' }


nextRegisterNumber :: CodegenFunction Word
nextRegisterNumber = do

  n <- gets _namedInstructionCount
  let m = n + 1

  modify $ \s -> s { _namedInstructionCount = m }

  return m


addTerminator :: AST.Named AST.Terminator -> CodegenFunction ()
addTerminator trm = do

  currentBasicBlock' <- gets _currentBasicBlock
  let currentBasicBlock'' = currentBasicBlock' { _terminator = Just trm }

  modify $ \s -> s { _currentBasicBlock = currentBasicBlock'' }


addLocalReference :: (S.Name, S.Type) -> CodegenFunction AST.Operand
addLocalReference (name, argType) = do

  let newSymbol = AST.LocalReference (typeToType argType) (AST.Name $ B.toShort $ BC.pack name)

  modify $ \s -> s { _symbols = M.insert name newSymbol (_symbols s) }

  return newSymbol


getLocalReference :: String -> CodegenFunction (Maybe AST.Operand)
getLocalReference name = do
  symbols' <- gets _symbols
  return $ M.lookup name symbols'


typeToType :: S.Type -> AST.Type
typeToType S.TypeUnit = AST.VoidType
typeToType S.TypeFloat = double
typeToType S.TypeInt = integer
typeToType t = error $ "type not implemented: " ++ show t


integer :: AST.Type
integer = AST.IntegerType 32


double :: AST.Type
double = AST.FloatingPointType AST.DoubleFP

