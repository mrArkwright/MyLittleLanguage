module Typecheck (typecheckProgram) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M

import Misc
import Syntax
import Builtins



data Context = Context {
    _contextVariables :: M.Map Name Type
  }

emptyContext :: Context
emptyContext = Context M.empty


type SymbolTable = M.Map Symbol FuncSignature


typecheckProgram :: MonadError Error m => [Def ()] -> m [Def Type]
typecheckProgram definitions = do

  let builtinsSymbolTable = M.fromList $ map (\(FuncDecl symbol signature) -> (symbol, signature)) (builtins ++ libraryBuiltins)

  symbolTable <- execStateT' builtinsSymbolTable $ forM definitions $ \definition -> do

    let (FuncDecl fSymbol fSignature) = defToFuncDecl definition

    symbolTable' <- get
    when (M.member fSymbol symbolTable') $ throwError ("function \"" ++ symbolToString fSymbol ++ "\" redefined", Just $ locFromDef definition)

    put $ M.insert fSymbol fSignature symbolTable'

  runReaderT' symbolTable $ mapM (evalStateT' emptyContext . typecheckDef) definitions


typecheckDef :: (MonadState Context m, MonadReader SymbolTable m, MonadError Error m) => Def () -> m (Def Type)
typecheckDef (Function symbol defType params expr loc) = do

  let params' = map (\(paramName, paramType) -> VarDecl paramName paramType loc) params
  mapM_ addVariable params'

  (typedExpr, exprType) <- typecheckExpr expr

  when (defType /= exprType) $ throwError ("definition of \"" ++ symbolToString symbol ++ "\": expected " ++ show defType ++ " but got " ++ show exprType, Just loc)

  return $ Function symbol defType params typedExpr loc


typecheckStatement :: (MonadState Context m, MonadReader SymbolTable m, MonadError Error m) => Statement () -> m (Statement Type, Type)
typecheckStatement (Expr expr () loc) = do
  (typedExpr, exprType) <- typecheckExpr expr
  return $ (Expr typedExpr exprType loc, exprType)

typecheckStatement (Let name letType expr () loc) = do

  (typedExpr, exprType) <- typecheckExpr expr

  when (letType /= exprType) $ throwError ("definition of \"" ++ name ++ "\": expected " ++ show letType ++ " but got " ++ show exprType, Just loc)

  addVariable $ VarDecl name letType loc

  return (Let name letType typedExpr TypeUnit loc, TypeUnit)


typecheckExpr :: (MonadState Context m, MonadReader SymbolTable m, MonadError Error m) => Expr () -> m (Expr Type, Type)
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

  return (Call symbol typedExprs calleeType loc, calleeType) where

    typecheckParameter :: (MonadState Context m, MonadReader SymbolTable m, MonadError Error m) => (Type, Type) -> m ()
    typecheckParameter (parameterType, exprType) = when (parameterType /= exprType) $ throwError ("call of function" ++ _symbolName symbol ++ ": expected " ++ show parameterType ++ " but got " ++ show exprType, Just loc)

typecheckExpr (Do statements _ loc) = do

  (typedStatements, statementTypes) <- fmap unzip $ mapM typecheckStatement statements

  case lastMaybe statementTypes of
   Just lastStatementType -> return (Do typedStatements lastStatementType loc, lastStatementType)
   Nothing -> throwError ("empty do block", Just loc)


findDefinition :: (MonadState Context m, MonadReader SymbolTable m, MonadError Error m) => Symbol -> m (Maybe FuncSignature)
findDefinition symbol = do
  symbolTable <- ask
  return $ M.lookup symbol symbolTable


findVariable :: (MonadState Context m, MonadReader SymbolTable m, MonadError Error m) => Name -> m (Maybe Type)
findVariable name = do
  variables <- getVariables
  return $ M.lookup name variables


getVariables :: (MonadState Context m, MonadReader SymbolTable m, MonadError Error m) => m (M.Map Name Type)
getVariables = gets _contextVariables


addVariable :: (MonadState Context m, MonadReader SymbolTable m, MonadError Error m) => VarDecl -> m ()
addVariable (VarDecl variableName variableType loc) = do

  variables <- getVariables

  when (M.member variableName variables) $ throwError ("variable redefined: " ++ variableName, Just loc)

  modify $ \context -> context { _contextVariables = M.insert variableName variableType variables }
