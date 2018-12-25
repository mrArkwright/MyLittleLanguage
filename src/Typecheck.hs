module Typecheck (typecheckProgram) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Morph

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


typecheckProgram :: Monad m => [Def] -> ExceptT Error m ()
typecheckProgram definitions = hoist generalize $ do
  let builtinsSymbolTable = M.fromList $ map (\(FuncDecl name signature) -> (name, signature)) (builtins ++ libraryBuiltins)
  symbolTable <- flip execStateT builtinsSymbolTable $ forM definitions $ \definition -> do
    let (FuncDecl fName fSignature) = defToFuncDecl definition
    symbolTable' <- get
    when (M.member fName symbolTable') $ throwError $ locDescription (locFromDef definition) ++ "function \"" ++ fName ++ "\" redefined"
    put $ M.insert fName fSignature symbolTable'

  flip runReaderT symbolTable $ mapM_ (flip evalStateT emptyContext . typecheckDef) definitions

typecheckDef :: Def -> Typecheck ()
typecheckDef (Function name defType params expr loc) = do
  let params' = map (\(paramName, paramType) -> VarDecl paramName paramType loc) params
  mapM_ addVariable params'
  exprType <- typecheckExpr expr
  when (defType /= exprType) $ throwError $ locDescription loc ++ "definition of \"" ++ name ++ "\": expected " ++ show defType ++ " but got " ++ show exprType

typecheckStatement :: Statement -> Typecheck Type
typecheckStatement (Expr expr _) =
  typecheckExpr expr
typecheckStatement (Let name letType expr loc) = do
  exprType <- typecheckExpr expr
  when (letType /= exprType) $ throwError $ locDescription loc ++ "definition of \"" ++ name ++ "\": expected " ++ show letType ++ " but got " ++ show exprType
  addVariable $ VarDecl name letType loc
  return TypeUnit

typecheckExpr :: Expr -> Typecheck Type
typecheckExpr (Unit _) = return TypeUnit
typecheckExpr (Int _ _) = return TypeInt
typecheckExpr (Float _ _) = return TypeFloat
typecheckExpr (Var name loc) = do
  variable <- findVariable name
  case variable of
    Just variableType -> return variableType
    Nothing -> throwError $ locDescription loc ++ "variable \"" ++ name ++ "\" not found"
typecheckExpr (If conditionExpr trueExpr falseExpr loc) = do
  conditionExprType <- typecheckExpr conditionExpr
  when (conditionExprType /= TypeBoolean) $ throwError $ locDescription loc ++ "condition expression must be of type " ++ show TypeBoolean ++ " but has type " ++ show conditionExprType
  trueExprType <- typecheckExpr trueExpr
  falseExprType <- typecheckExpr falseExpr
  when (trueExprType /= falseExprType) $ throwError $ locDescription loc ++ "types of if branches differ: " ++ show trueExprType ++ " and " ++ show falseExprType
  return trueExprType
typecheckExpr (Call cName exprs loc) = do
  declaration <- findDefinition cName
  FuncSignature calleeType calleeParameters <- case declaration of
    Just signature -> return signature
    Nothing -> throwError $ locDescription loc ++ "function not found: " ++ cName
  exprTypes <- mapM typecheckExpr exprs
  mapM_ typecheckParameter $ zip calleeParameters exprTypes
  return calleeType
    where
      typecheckParameter :: (Type, Type) -> Typecheck ()
      typecheckParameter (parameterType, exprType) = when (parameterType /= exprType) $ throwError $ locDescription loc ++ "function call: expected " ++ show parameterType ++ " but got " ++ show exprType
typecheckExpr (Do statements loc) = do
  statementTypes <- mapM typecheckStatement statements
  case lastMaybe statementTypes of
   Just lastStatementType -> return lastStatementType
   Nothing -> throwError $ locDescription loc ++ "empty do block"


findDefinition :: Name -> Typecheck (Maybe FuncSignature)
findDefinition name = do
  definitions <- ask
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
  when (M.member variableName variables) $ throwError $ locDescription loc ++ "variable redefined: " ++ variableName
  modify $ \context -> context { contextVariables = M.insert variableName variableType variables }

