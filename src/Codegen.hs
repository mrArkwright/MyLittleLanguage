module Codegen (initModule, codegen) where


import qualified Data.Map as Map
import qualified Data.ByteString.Short as B (toShort)
import qualified Data.ByteString.Char8 as BC

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Morph

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as AST
import qualified LLVM.AST.Constant as AST.C
import qualified LLVM.AST.Float as AST
import qualified LLVM.AST.FloatingPointPredicate as AST
import qualified LLVM.AST.CallingConvention as AST
import qualified LLVM.AST.Type as AST
import LLVM.AST.Instruction ( Named( (:=) ) )

import Misc
import Builtins
import qualified Syntax as S



--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

type Codegen = StateT AST.Module (Except Error)

initModule :: String -> AST.Module
initModule name = AST.defaultModule {
    AST.moduleName = B.toShort $ BC.pack name
  }

codegen :: Monad m => AST.Module -> [S.Def] -> ExceptT Error m AST.Module
codegen astModule definitions = hoist generalize $ flip execStateT astModule $ do
  let codegenBuiltin (Builtin builtinName args) = addGlobalFunction builtinName args []
  mapM_ codegenBuiltin builtins
  mapM_ codegenDefinition definitions

codegenDefinition :: S.Def -> Codegen ()
codegenDefinition (S.Function _ name _ args body) = do
  let argNames = map fst args
  basicBlocks <- lift $ codegenFunction argNames body
  addGlobalFunction name argNames basicBlocks

addGlobalFunction :: String -> [String] -> [AST.BasicBlock] -> Codegen ()
addGlobalFunction name argNames basicBlocks = do
  let args = map (\argName -> AST.Parameter double (AST.Name argName) []) (map (B.toShort . BC.pack) argNames)
  let def = AST.GlobalDefinition $ AST.functionDefaults {
    AST.name        = AST.Name (B.toShort $ BC.pack name),
    AST.parameters  = (args, False),
    AST.returnType  = double,
    AST.basicBlocks = basicBlocks
  }
  defs <- gets AST.moduleDefinitions
  modify $ \s -> s { AST.moduleDefinitions = defs ++ [def] }



--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

data Function = Function {
    _currentBasicBlock     :: BasicBlock,
    _basicBlocks           :: [BasicBlock],
    _symbols               :: Map.Map String AST.Operand,
    _namedInstructionCount :: Word,
    _labels                :: Map.Map String Int
  } deriving Show

emptyFunction :: Function
emptyFunction = Function {
    _currentBasicBlock     = emptyBasicBlock "entry",
    _basicBlocks           = [],
    _symbols               = Map.empty,
    _namedInstructionCount = 0,
    _labels                = Map.empty
  }

data BasicBlock = BasicBlock {
    _name         :: String,
    _instructions :: [AST.Named AST.Instruction],
    _terminator   :: Maybe (AST.Named AST.Terminator)
  } deriving Show

emptyBasicBlock :: String -> BasicBlock
emptyBasicBlock name = BasicBlock {
    _name         =  name,
    _instructions = [],
    _terminator   = Nothing
  }

type CodegenFunction = StateT Function (Except Error)

codegenFunction :: [String] -> S.Expr -> Except Error [AST.BasicBlock]
codegenFunction argNames body = (flip evalStateT) emptyFunction $ do
  mapM_ addLocalReference argNames
  result <- codegenExpression body
  ret result

  basicBlocks' <- gets _basicBlocks
  currentBasicBlock' <- gets _currentBasicBlock
  modify $ \s -> s { _basicBlocks = basicBlocks' ++ [currentBasicBlock'] }

  basicBlocks'' <- gets _basicBlocks
  mapM basicBlockToLLVMBasicBlock basicBlocks''

basicBlockToLLVMBasicBlock :: BasicBlock -> CodegenFunction AST.BasicBlock
basicBlockToLLVMBasicBlock (BasicBlock name' instructions' terminator') = do
  terminator'' <- maybeToExcept terminator' $ "Block has no terminator: " ++ (show name')
  return $ AST.BasicBlock (AST.Name $ B.toShort $ BC.pack name') instructions' terminator''

newBasicBlock :: String -> CodegenFunction ()
newBasicBlock name = do
  basicBlocks' <- gets _basicBlocks
  currentBasicBlock' <- gets _currentBasicBlock
  modify $ \s -> s { _basicBlocks = basicBlocks' ++ [currentBasicBlock'] }
  modify $ \s -> s { _currentBasicBlock = emptyBasicBlock name }

makeUniqueLabel :: String -> CodegenFunction String
makeUniqueLabel label = do
  labels' <- gets _labels
  case Map.lookup label labels' of
    Nothing    -> do
      modify $ \s -> s { _labels = Map.insert label 1 labels' }
      return label
    Just count -> do
      modify $ \s -> s { _labels = Map.insert label (count + 1) labels' }
      return $ label ++ show count


--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

codegenExpression :: S.Expr -> CodegenFunction AST.Operand
codegenExpression (S.Unit _) = return $ AST.ConstantOperand $ AST.C.Int 64 0 -- TODO remove dummy value for unit
codegenExpression (S.Int _ value) = return $ AST.ConstantOperand $ AST.C.Int 64 value
codegenExpression (S.Float _ value) = return $ AST.ConstantOperand $ AST.C.Float (AST.Double value)
codegenExpression (S.Var _ name) = do
  reference <- getLocalReference name
  return $ maybeError reference ("no such symbol: " ++ name)
codegenExpression (S.If _ condition ifTrue ifFalse) = do
  thenLabel <- makeUniqueLabel "if.then"
  elseLabel <- makeUniqueLabel "if.else"
  contLabel <- makeUniqueLabel "if.cont"

  conditionResult <- codegenExpression condition
  condBr conditionResult thenLabel elseLabel

  newBasicBlock thenLabel
  trueResult <- codegenExpression ifTrue
  br contLabel

  newBasicBlock elseLabel
  falseResult <- codegenExpression ifFalse
  br contLabel

  newBasicBlock contLabel
  phi [(trueResult, thenLabel), (falseResult, elseLabel)]
codegenExpression (S.Call _ name argExprs) = do
  args <- mapM codegenExpression argExprs
  case name of
    "+" -> do
      let (a:b:_) = args
      fadd a b
    "-" -> do
      let (a:b:_) = args
      fsub a b
    "*" -> do
      let (a:b:_) = args
      fmul a b
    "/" -> do
      let (a:b:_) = args
      fdiv a b
    "<" -> do
      let (a:b:_) = args
      fcmp AST.ULT a b
    _   -> do
      let functionType = AST.ptr $ AST.FunctionType double (replicate (length args) double) False
      let function = AST.ConstantOperand $ AST.C.GlobalReference functionType (AST.Name $ B.toShort $ BC.pack name)
      call function args
codegenExpression (S.Do _ statements) = do
  results <- mapM codegenStatement statements
  return $ head results

codegenStatement :: S.Statement -> CodegenFunction AST.Operand
codegenStatement (S.Expr _ expression) = codegenExpression expression
codegenStatement (S.Let _ name _ expression) = do
  result <- codegenExpression expression
  modify $ \s -> s { _symbols = Map.insert name result (_symbols s) }
  return result

fadd :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fadd a b = addNamedInstruction $ AST.FAdd AST.noFastMathFlags a b []

fsub :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fsub a b = addNamedInstruction $ AST.FSub AST.noFastMathFlags a b []

fmul :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fmul a b = addNamedInstruction $ AST.FMul AST.noFastMathFlags a b []

fdiv :: AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fdiv a b = addNamedInstruction $ AST.FDiv AST.noFastMathFlags a b []

fcmp :: AST.FloatingPointPredicate -> AST.Operand -> AST.Operand -> CodegenFunction AST.Operand
fcmp condition a b = addNamedInstruction $ AST.FCmp condition a b []

call :: AST.Operand -> [AST.Operand] -> CodegenFunction AST.Operand
call fn args = addNamedInstruction $ AST.Call Nothing AST.C [] (Right fn) [(arg, []) | arg <- args] [] []

br :: String -> CodegenFunction ()
br label = addTerminator $ AST.Do $ AST.Br (AST.Name $ B.toShort $ BC.pack label) []

condBr :: AST.Operand -> String -> String -> CodegenFunction ()
condBr condition trueLabel falseLabel =
  let trueLabel'  = AST.Name $ B.toShort $ BC.pack trueLabel in
  let falseLabel' = AST.Name $ B.toShort $ BC.pack falseLabel in
  addTerminator $ AST.Do $ AST.CondBr condition trueLabel' falseLabel' []

phi :: [(AST.Operand, String)] -> CodegenFunction AST.Operand
phi incoming =
  let incoming' = map (\(operand, label) -> (operand, AST.Name $ B.toShort $ BC.pack label)) incoming in
  addNamedInstruction $ AST.Phi double incoming' []

ret :: AST.Operand -> CodegenFunction ()
ret val = addTerminator $ AST.Do $ AST.Ret (Just val) []

addNamedInstruction :: AST.Instruction -> CodegenFunction AST.Operand
addNamedInstruction instruction = do
  n <- nextRegisterNumber
  let ref = (AST.UnName n)
  currentBasicBlock' <- gets _currentBasicBlock
  let currentBasicBlock'' = currentBasicBlock' { _instructions = _instructions currentBasicBlock' ++ [ref := instruction] }
  modify $ \s -> s { _currentBasicBlock = currentBasicBlock'' }
  return $ AST.LocalReference double ref

--addUnnamedInstruction :: AST.Instruction -> State Function ()
--addUnnamedInstruction instruction = do
--  modify $ \s -> s { instructions = instructions s ++ [AST.Do instruction] }

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

addLocalReference :: String -> CodegenFunction AST.Operand
addLocalReference name = do
  let newSymbol = AST.LocalReference double (AST.Name $ B.toShort $ BC.pack name)
  modify $ \s -> s { _symbols = Map.insert name newSymbol (_symbols s) }
  return newSymbol

getLocalReference :: String -> CodegenFunction (Maybe AST.Operand)
getLocalReference name = do
  symbols' <- gets _symbols
  return $ Map.lookup name symbols'

double :: AST.Type
double = AST.FloatingPointType AST.DoubleFP

