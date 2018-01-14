module Codegen where


import Data.List
import Data.Maybe
import Data.Function
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.ByteString.Short as B (toShort, fromShort)
import qualified Data.ByteString.Char8 as BC

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

initModule :: String -> AST.Module
initModule name =
  let codegenBuiltin (Builtin name args) = addGlobalFunction name args [] in
  let addBuiltins = execState (mapM codegenBuiltin builtins) in
  addBuiltins $ AST.defaultModule { AST.moduleName = B.toShort $ BC.pack name }

codegen :: AST.Module -> [S.Def] -> AST.Module
codegen astModule definitions =
  execState (mapM codegenDefinition definitions) astModule

codegenDefinition :: S.Def -> State AST.Module ()
codegenDefinition (S.Function name argNames body) = do
  let basicBlocks = codegenFunction argNames body
  addGlobalFunction name argNames basicBlocks

addGlobalFunction :: String -> [String] -> [AST.BasicBlock] -> State AST.Module ()
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
  currentBasicBlock     :: BasicBlock,
  basicBlocks           :: [BasicBlock],
  symbols               :: Map.Map String AST.Operand,
  namedInstructionCount :: Word,
  labels                :: Map.Map String Int
  } deriving Show

emptyFunction :: Function
emptyFunction = Function {
  currentBasicBlock     = emptyBasicBlock "entry",
  basicBlocks           = [],
  symbols               = Map.empty,
  namedInstructionCount = 0,
  labels                = Map.empty
  }

data BasicBlock = BasicBlock {
  name         :: String,
  instructions :: [AST.Named AST.Instruction],
  terminator   :: Maybe (AST.Named AST.Terminator)
  } deriving Show

emptyBasicBlock :: String -> BasicBlock
emptyBasicBlock name = BasicBlock {
  name         =  name,
  instructions = [],
  terminator   = Nothing
  }

codegenFunction :: [String] -> S.Expr -> [AST.BasicBlock]
codegenFunction argNames body = (flip evalState) emptyFunction $ do
  args <- mapM addLocalReference argNames
  result <- codegenExpression body
  ret result

  basicBlocks' <- gets basicBlocks
  currentBasicBlock' <- gets currentBasicBlock
  modify $ \s -> s { basicBlocks = basicBlocks' ++ [currentBasicBlock'] }

  basicBlocks'' <- gets basicBlocks
  return $ map basicBlockToLLVMBasicBlock basicBlocks''

basicBlockToLLVMBasicBlock :: BasicBlock -> AST.BasicBlock
basicBlockToLLVMBasicBlock (BasicBlock name' instructions' terminator') =
  let terminator'' = maybeError terminator' ("Block has no terminator: " ++ (show name')) in
  AST.BasicBlock (AST.Name $ B.toShort $ BC.pack name') instructions' terminator''

newBasicBlock :: String -> State Function ()
newBasicBlock name = do
  basicBlocks' <- gets basicBlocks
  currentBasicBlock' <- gets currentBasicBlock
  modify $ \s -> s { basicBlocks = basicBlocks' ++ [currentBasicBlock'] }
  modify $ \s -> s { currentBasicBlock = emptyBasicBlock name }

makeUniqueLabel :: String -> State Function String
makeUniqueLabel label = do
  labels' <- gets labels
  case Map.lookup label labels' of
    Nothing    -> do
      modify $ \s -> s { labels = Map.insert label 1 labels' }
      return label
    Just count -> do
      modify $ \s -> s { labels = Map.insert label (count + 1) labels' }
      return $ label ++ show count


--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

codegenExpression :: S.Expr -> State Function AST.Operand
codegenExpression (S.Int value) = return $ AST.ConstantOperand $ AST.C.Int 64 value
codegenExpression (S.Float value) = return $ AST.ConstantOperand $ AST.C.Float (AST.Double value)
codegenExpression (S.Var name) = do
  reference <- getLocalReference name
  return $ maybeError reference ("no such symbol: " ++ name)
codegenExpression (S.If condition ifTrue ifFalse) = do
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
codegenExpression (S.Call name argExprs) = do
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
codegenExpression (S.Do statements) = do
  results <- mapM codegenStatement statements
  return $ head results

codegenStatement :: S.Statement -> State Function AST.Operand
codegenStatement (S.Expr expression) = codegenExpression expression
codegenStatement (S.Let name expression) = do
  result <- codegenExpression expression
  modify $ \s -> s { symbols = Map.insert name result (symbols s) }
  return result

fadd :: AST.Operand -> AST.Operand -> State Function AST.Operand
fadd a b = addNamedInstruction $ AST.FAdd AST.NoFastMathFlags a b []

fsub :: AST.Operand -> AST.Operand -> State Function AST.Operand
fsub a b = addNamedInstruction $ AST.FSub AST.NoFastMathFlags a b []

fmul :: AST.Operand -> AST.Operand -> State Function AST.Operand
fmul a b = addNamedInstruction $ AST.FMul AST.NoFastMathFlags a b []

fdiv :: AST.Operand -> AST.Operand -> State Function AST.Operand
fdiv a b = addNamedInstruction $ AST.FDiv AST.NoFastMathFlags a b []

fcmp :: AST.FloatingPointPredicate -> AST.Operand -> AST.Operand -> State Function AST.Operand
fcmp condition a b = addNamedInstruction $ AST.FCmp condition a b []

call :: AST.Operand -> [AST.Operand] -> State Function AST.Operand
call fn args = addNamedInstruction $ AST.Call Nothing AST.C [] (Right fn) [(arg, []) | arg <- args] [] []

br :: String -> State Function ()
br label = addTerminator $ AST.Do $ AST.Br (AST.Name $ B.toShort $ BC.pack label) []

condBr :: AST.Operand -> String -> String -> State Function ()
condBr condition trueLabel falseLabel =
  let trueLabel'  = AST.Name $ B.toShort $ BC.pack trueLabel in
  let falseLabel' = AST.Name $ B.toShort $ BC.pack falseLabel in
  addTerminator $ AST.Do $ AST.CondBr condition trueLabel' falseLabel' []

phi :: [(AST.Operand, String)] -> State Function AST.Operand
phi incoming =
  let incoming' = map (\(operand, label) -> (operand, AST.Name $ B.toShort $ BC.pack label)) incoming in
  addNamedInstruction $ AST.Phi double incoming' []

ret :: AST.Operand -> State Function ()
ret val = addTerminator $ AST.Do $ AST.Ret (Just val) []

addNamedInstruction :: AST.Instruction -> State Function AST.Operand
addNamedInstruction instruction = do
  n <- nextRegisterNumber
  let ref = (AST.UnName n)
  currentBasicBlock' <- gets currentBasicBlock
  let currentBasicBlock'' = currentBasicBlock' { instructions = instructions currentBasicBlock' ++ [ref := instruction] }
  modify $ \s -> s { currentBasicBlock = currentBasicBlock'' }
  return $ AST.LocalReference double ref

--addUnnamedInstruction :: AST.Instruction -> State Function ()
--addUnnamedInstruction instruction = do
--  modify $ \s -> s { instructions = instructions s ++ [AST.Do instruction] }

nextRegisterNumber :: State Function Word
nextRegisterNumber = do
  n <- gets namedInstructionCount
  let m = n + 1
  modify $ \s -> s { namedInstructionCount = m }
  return m

addTerminator :: AST.Named AST.Terminator -> State Function ()
addTerminator trm = do
  currentBasicBlock' <- gets currentBasicBlock
  let currentBasicBlock'' = currentBasicBlock' { terminator = Just trm }
  modify $ \s -> s { currentBasicBlock = currentBasicBlock'' }

addLocalReference :: String -> State Function AST.Operand
addLocalReference name = do
  let newSymbol = AST.LocalReference double (AST.Name $ B.toShort $ BC.pack name)
  modify $ \s -> s { symbols = Map.insert name newSymbol (symbols s) }
  return newSymbol

getLocalReference :: String -> State Function (Maybe AST.Operand)
getLocalReference name = do
  symbols' <- gets symbols
  return $ Map.lookup name symbols'

integer :: AST.Type
integer = AST.IntegerType 64

double :: AST.Type
double = AST.FloatingPointType AST.DoubleFP

