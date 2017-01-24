module Codegen where


import Data.List
import Data.Maybe
import Data.Function
import Control.Monad.State
import qualified Data.Map as Map

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as AST
import qualified LLVM.General.AST.Constant as AST.C
import qualified LLVM.General.AST.Float as AST
import qualified LLVM.General.AST.CallingConvention as AST
import LLVM.General.AST.Instruction ( Named( (:=) ) )

import Misc
import qualified Syntax as S



--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

codegen :: AST.Module -> [S.Def] -> AST.Module
codegen astModule definitions =
  let moduleState = mapM codegenDefinition definitions in
  execState moduleState astModule

codegenDefinition :: S.Def -> State AST.Module ()
codegenDefinition (S.Function name argNames body) = do
  let basicBlocks = codegenFunction argNames body
  addGlobalFunction name argNames basicBlocks
codegenDefinition (S.Extern name argNames) = do
  addGlobalFunction name argNames []

addGlobalFunction :: String -> [String] -> [AST.BasicBlock] -> State AST.Module ()
addGlobalFunction name argNames basicBlocks = do
  let args = map (\argName -> AST.Parameter double (AST.Name argName) []) argNames
  let def = AST.GlobalDefinition $ AST.functionDefaults {
    AST.name        = AST.Name name,
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
  blockName             :: AST.Name,
  instructions          :: [AST.Named AST.Instruction],
  terminator            :: Maybe (AST.Named AST.Terminator),
  namedInstructionCount :: Word,
  symbols               :: Map.Map String AST.Operand
  } deriving Show

emptyFunction :: Function
emptyFunction = Function {
  blockName             = AST.Name "entry",
  instructions          = [],
  terminator            = Nothing,
  namedInstructionCount = 0,
  symbols               = Map.empty
  }

codegenFunction :: [String] -> S.Expr -> [AST.BasicBlock]
codegenFunction argNames body = fst $ runState' emptyFunction $ do
  args <- mapM addLocalReference argNames
  codegenExpression body >>= ret

  blockName' <- gets blockName
  instructions' <- gets instructions
  terminator' <- gets terminator
  let terminator'' = maybeError terminator' ("Block has no terminator: " ++ (show blockName'))
  return $ [AST.BasicBlock blockName' instructions' terminator'']



--------------------------------------------------------------------------------
-- Instructions
--------------------------------------------------------------------------------

codegenExpression :: S.Expr -> State Function AST.Operand
codegenExpression (S.Float value) = return $ AST.ConstantOperand $ AST.C.Float (AST.Double value)
codegenExpression (S.Var name) = do
  reference <- getLocalReference name
  return $ maybeError reference ("no such symbol: " ++ name)
codegenExpression (S.Call name argExprs) = do
  args <- mapM codegenExpression argExprs
  case name of
    "+" -> do
      let (a:b:_) = args
      fadd a b
    "*" -> do
      let (a:b:_) = args
      fmul a b
    _   -> do
      let function = AST.ConstantOperand $ AST.C.GlobalReference double (AST.Name name)
      call function args

fadd :: AST.Operand -> AST.Operand -> State Function AST.Operand
fadd a b = addNamedInstruction $ AST.FAdd AST.NoFastMathFlags a b []

fmul :: AST.Operand -> AST.Operand -> State Function AST.Operand
fmul a b = addNamedInstruction $ AST.FMul AST.NoFastMathFlags a b []

call :: AST.Operand -> [AST.Operand] -> State Function AST.Operand
call fn args = addNamedInstruction $ AST.Call Nothing AST.C [] (Right fn) [(arg, []) | arg <- args] [] []

ret :: AST.Operand -> State Function (AST.Named AST.Terminator)
ret val = addTerminator $ AST.Do $ AST.Ret (Just val) []

addNamedInstruction :: AST.Instruction -> State Function AST.Operand
addNamedInstruction instruction = do
  n <- nextRegisterNumber
  let ref = (AST.UnName n)
  modify $ \s -> s { instructions = instructions s ++ [ref := instruction] }
  return $ AST.LocalReference double ref

addUnnamedInstruction :: AST.Instruction -> State Function ()
addUnnamedInstruction instruction = do
  modify $ \s -> s { instructions = instructions s ++ [AST.Do instruction] }

nextRegisterNumber :: State Function Word
nextRegisterNumber = do
  n <- gets namedInstructionCount
  let m = n + 1
  modify $ \s -> s { namedInstructionCount = m }
  return m

addTerminator :: AST.Named AST.Terminator -> State Function (AST.Named AST.Terminator)
addTerminator trm = do
  modify $ \s -> s { terminator = Just trm }
  return trm

addLocalReference :: String -> State Function AST.Operand
addLocalReference name = do
  let newSymbol = AST.LocalReference double (AST.Name name)
  modify $ \s -> s { symbols = Map.insert name newSymbol (symbols s) }
  return newSymbol

getLocalReference :: String -> State Function (Maybe AST.Operand)
getLocalReference name = do
  symbols' <- gets symbols
  return $ Map.lookup name symbols'

double :: AST.Type
double = AST.FloatingPointType 64 AST.IEEE

