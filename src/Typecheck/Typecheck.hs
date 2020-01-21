module Typecheck.Typecheck (typecheck) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M

import Misc
import Parse.Syntax (Name, Type(..), Parameter(..))
import Rename.Syntax (Symbol(..), GlobalSymbol(..), LocalSymbol(..))
import qualified Rename.Syntax as Rename
import Typecheck.Syntax
import Builtins



type SymbolTable = M.Map Symbol Type


typecheck :: MonadError Error m => [Rename.GlobalDefinition] -> m [GlobalDefinition]
typecheck definitions = evalStateT' M.empty $ do

  mapM_ importBuiltin $ builtins ++ libraryBuiltins

  mapM_ importDefinition $ definitions

  mapM typecheckGlobalDefinition definitions


importBuiltin :: (MonadState SymbolTable m, MonadError Error m) => (Name, Type) -> m ()
importBuiltin (name, type_) = do
  let symbol' = SymbolGlobal $ GlobalSymbol name []
  modify $ M.insert symbol' type_


importDefinition :: (MonadState SymbolTable m, MonadError Error m) => Rename.GlobalDefinition -> m ()
importDefinition definition = modify $ M.insert (SymbolGlobal $ Rename.globalDefinitionSymbol definition) (Rename.globalDefinitionType definition)


typecheckGlobalDefinition :: (MonadState SymbolTable m, MonadError Error m) => Rename.GlobalDefinition -> m GlobalDefinition
typecheckGlobalDefinition (Rename.GlobalDefinitionValue (Rename.GlobalValueDefinition symbol type_ expression loc)) = do

  (typedExpression, expressionType) <- typecheckExpression expression

  when (type_ /= expressionType) $ throwError ("(Typecheck) definition of \"" ++ show symbol ++ "\": expected " ++ show type_ ++ " but got " ++ show expressionType, Just loc)

  return $ GlobalDefinitionValue $ GlobalValueDefinition symbol type_ typedExpression loc

typecheckGlobalDefinition (Rename.GlobalDefinitionFunction (Rename.FunctionDefinition symbol parameters resultType expression loc)) = do

  mapM_ addParameter parameters

  (typedExpression, expressionType) <- typecheckExpression expression

  when (resultType /= expressionType) $ throwError ("(Typecheck) definition of \"" ++ show symbol ++ "\": expected " ++ show resultType ++ " but got " ++ show expressionType, Just loc)

  return $ GlobalDefinitionFunction $ FunctionDefinition symbol parameters resultType typedExpression loc


addParameter :: (MonadState SymbolTable m, MonadError Error m) => Parameter -> m ()
addParameter parameter = do
  let symbol = LocalSymbol (parameter_name parameter)
  let type_ = parameter_type parameter
  modify $ M.insert (SymbolLocal symbol) type_


typecheckStatement :: (MonadState SymbolTable m, MonadError Error m) => Rename.Statement -> m (Statement, Type)
typecheckStatement (Rename.StatementExpression expression loc) = do
  (typedExpression, expressionType) <- typecheckExpression expression
  return (StatementExpression typedExpression expressionType loc, expressionType)

typecheckStatement (Rename.StatementDefinition definition loc) = do

  typedDefinition <- typecheckLocalValueDefinition definition

  modify $ M.insert (SymbolLocal $ Rename.localValueDefinition_symbol definition) (Rename.localValueDefinition_type definition)

  return (StatementDefinition typedDefinition TypeUnit loc, TypeUnit)


typecheckLocalValueDefinition :: (MonadState SymbolTable m, MonadError Error m) => Rename.LocalValueDefinition -> m LocalValueDefinition
typecheckLocalValueDefinition (Rename.LocalValueDefinition symbol type_ expression loc) = do

  (typedExpression, expressionType) <- typecheckExpression expression

  when (type_ /= expressionType) $ throwError ("(Typecheck) definition of \"" ++ show symbol ++ "\": expected " ++ show type_ ++ " but got " ++ show expressionType, Just loc)

  return $ LocalValueDefinition symbol type_ typedExpression loc


typecheckExpression :: (MonadState SymbolTable m, MonadError Error m) => Rename.Expression -> m (Expression, Type)
typecheckExpression (Rename.Unit loc) = return (Unit TypeUnit loc, TypeUnit)

typecheckExpression (Rename.Int value loc) = return (Int value TypeInt loc, TypeInt)

typecheckExpression (Rename.Float value loc) = return (Float value TypeFloat loc, TypeFloat)

typecheckExpression (Rename.SymbolReference symbol loc) = do

  symbolType <- findSymbol symbol

  case symbolType of
    Just symbolType' -> return (SymbolReference symbol symbolType' loc, symbolType')
    Nothing -> throwError ("(Typecheck) symbol \"" ++ show symbol ++ "\" not found", Just loc)

typecheckExpression (Rename.Call symbol arguments loc) = do

  symbolType <- findSymbol symbol

  (parameterTypes, resultType) <- case symbolType of
    Just (TypeFunction parameterTypes' resultType') -> return (parameterTypes', resultType')
    Just _ -> throwError ("(Typecheck) can't call non-function symbol: " ++ show symbol, Just loc)
    Nothing -> throwError ("(Typecheck) function not found: " ++ show symbol, Just loc)

  (typedArguments, argumentTypes) <- fmap unzip $ mapM typecheckExpression arguments

  let numberArgumentsExpected = length parameterTypes
  let numberArgumentsPassed = length argumentTypes
  when (numberArgumentsExpected /= numberArgumentsPassed) $ throwError ("(Typecheck) call of function " ++ show symbol ++ ": wrong number of arguments. expected: " ++ show numberArgumentsExpected ++ ", passed: " ++ show numberArgumentsPassed, Just loc)

  mapM_ typecheckParameter $ zip parameterTypes argumentTypes

  return (Call symbol typedArguments resultType loc, resultType) where

    typecheckParameter :: (MonadState SymbolTable m, MonadError Error m) => (Type, Type) -> m ()
    typecheckParameter (parameterType, argumentType) = when (parameterType /= argumentType) $ throwError ("(Typecheck) call of function" ++ show symbol ++ ": expected " ++ show parameterType ++ " but got " ++ show argumentType, Just loc)

typecheckExpression (Rename.If conditionExpression trueExpression falseExpression loc) = do

  (typedConditionExpression, conditionExpressionType) <- typecheckExpression conditionExpression

  when (conditionExpressionType /= TypeBoolean) $ throwError ("(Typecheck) condition expression must be of type " ++ show TypeBoolean ++ " but has type " ++ show conditionExpressionType, Just loc)

  (typedTrueExpression, trueExpressionType) <- typecheckExpression trueExpression
  (typedFalseExpression, falseExpressionType) <- typecheckExpression falseExpression

  when (trueExpressionType /= falseExpressionType) $ throwError ("(Typecheck) types of if branches differ: " ++ show trueExpressionType ++ " and " ++ show falseExpressionType, Just loc)

  return (If typedConditionExpression typedTrueExpression typedFalseExpression trueExpressionType loc, trueExpressionType)


typecheckExpression (Rename.Do statements loc) = do

  (typedStatements, statementTypes) <- mapAndUnzipM typecheckStatement statements

  case lastMaybe statementTypes of
   Just lastStatementType -> return (Do typedStatements lastStatementType loc, lastStatementType)
   Nothing -> throwError ("(Typecheck) empty do block", Just loc)


findSymbol :: (MonadState SymbolTable m, MonadError Error m) => Symbol -> m (Maybe Type)
findSymbol symbol = gets $ M.lookup symbol
