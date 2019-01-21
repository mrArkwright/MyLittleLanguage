module Typecheck (typecheckProgram) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Morph

import Data.List
import qualified Data.Map as M

import Misc
import Syntax
import Builtins


data Context = Context {
  contextVariables :: M.Map Name Type
}

emptyContext :: Context
emptyContext = Context M.empty

type SymbolTable = M.Map Name FuncSignature

type Typecheck = StateT Context (ReaderT SymbolTable (Except Error))


typecheckProgram :: Monad m => [Def ()] -> ExceptT Error m [Def Type]
typecheckProgram definitions = hoist generalize $ do
  let builtinsSymbolTable = M.fromList $ map (\(FuncDecl name signature) -> (name, signature)) (builtins ++ libraryBuiltins)
  symbolTable <- execStateT' builtinsSymbolTable $ forM definitions $ \definition -> do
    let (FuncDecl fName fSignature) = defToFuncDecl definition
    symbolTable' <- get
    when (M.member fName symbolTable') $ throwError ("function \"" ++ fName ++ "\" redefined", Just $ locFromDef definition)
    put $ M.insert fName fSignature symbolTable'

  runReaderT' symbolTable $ mapM (evalStateT' emptyContext . typecheckDef) definitions

typecheckDef :: Def () -> Typecheck (Def Type)
typecheckDef (Function name defType params expr loc) = do
  let params' = map (\(paramName, paramType) -> VarDecl paramName paramType loc) params
  mapM_ addVariable params'
  (typedExpr, exprType) <- typecheckExpr expr
  when (defType /= exprType) $ throwError ("definition of \"" ++ name ++ "\": expected " ++ show defType ++ " but got " ++ show exprType, Just loc)
  return $ Function name defType params typedExpr loc

typecheckStatement :: Statement () -> Typecheck (Statement Type, Type)
typecheckStatement (Expr expr () loc) = do
  (typedExpr, exprType) <- typecheckExpr expr
  return $ (Expr typedExpr exprType loc, exprType)
typecheckStatement (Let name letType expr () loc) = do
  (typedExpr, exprType) <- typecheckExpr expr
  when (letType /= exprType) $ throwError ("definition of \"" ++ name ++ "\": expected " ++ show letType ++ " but got " ++ show exprType, Just loc)
  addVariable $ VarDecl name letType loc
  return (Let name letType typedExpr TypeUnit loc, TypeUnit)

typecheckExpr :: Expr () -> Typecheck (Expr Type, Type)
typecheckExpr (Unit _ loc) = return (Unit TypeUnit loc, TypeUnit)
typecheckExpr (Int value _ loc) = return (Int value TypeInt loc, TypeInt)
typecheckExpr (Float value _ loc) = return (Float value TypeFloat loc, TypeFloat)
typecheckExpr (Var name _ loc) = do
  variable <- findVariable name
  case variable of
    Just variableType -> return (Var name variableType loc, variableType)
    Nothing -> throwError ("variable \"" ++ name ++ "\" not found", Just loc)
typecheckExpr (If conditionExpr trueExpr falseExpr _ loc) = do
  (typedConditionExpr, conditionExprType) <- typecheckExpr conditionExpr
  when (conditionExprType /= TypeBoolean) $ throwError ("condition expression must be of type " ++ show TypeBoolean ++ " but has type " ++ show conditionExprType, Just loc)
  (typedTrueExpr, trueExprType) <- typecheckExpr trueExpr
  (typedFalseExpr, falseExprType) <- typecheckExpr falseExpr
  when (trueExprType /= falseExprType) $ throwError ("types of if branches differ: " ++ show trueExprType ++ " and " ++ show falseExprType, Just loc)
  return (If typedConditionExpr typedTrueExpr typedFalseExpr trueExprType loc, trueExprType)
typecheckExpr (Call symbol exprs _ loc) = do
  declaration <- findDefinition symbol
  FuncSignature calleeType calleeParameters <- case declaration of
    Just signature -> return signature
    Nothing -> throwError ("function not found: " ++ _symbolName symbol, Just loc)
  (typedExprs, exprTypes) <- fmap unzip $ mapM typecheckExpr exprs
  let numberArgumentsPasses = length exprTypes
  let numberArgumentsExpected = length calleeParameters
  when (numberArgumentsPasses /= numberArgumentsExpected) $ throwError ("call of function " ++ _symbolName symbol ++ ": wrong number of arguments. expected: " ++ show numberArgumentsExpected ++ ", passed: " ++ show numberArgumentsPasses, Just loc)
  mapM_ typecheckParameter $ zip calleeParameters exprTypes
  return (Call symbol typedExprs calleeType loc, calleeType)
    where
      typecheckParameter :: (Type, Type) -> Typecheck ()
      typecheckParameter (parameterType, exprType) = when (parameterType /= exprType) $ throwError ("call of function" ++ _symbolName symbol ++ ": expected " ++ show parameterType ++ " but got " ++ show exprType, Just loc)
typecheckExpr (Do statements _ loc) = do
  (typedStatements, statementTypes) <- fmap unzip $ mapM typecheckStatement statements
  case lastMaybe statementTypes of
   Just lastStatementType -> return (Do typedStatements lastStatementType loc, lastStatementType)
   Nothing -> throwError ("empty do block", Just loc)


findDefinition :: Symbol -> Typecheck (Maybe FuncSignature)
findDefinition symbol = do
  definitions <- ask
  let name = intercalate "." $ _symbolPath symbol ++ [_symbolName symbol]
  return $ M.lookup name definitions

findVariable :: Name -> Typecheck (Maybe Type)
findVariable name = do
  variables <- getVariables
  return $ M.lookup name variables

getVariables :: Typecheck (M.Map Name Type)
getVariables = gets contextVariables

addVariable :: VarDecl -> Typecheck ()
addVariable (VarDecl variableName variableType loc) = do
  variables <- getVariables
  when (M.member variableName variables) $ throwError ("variable redefined: " ++ variableName, Just loc)
  modify $ \context -> context { contextVariables = M.insert variableName variableType variables }

