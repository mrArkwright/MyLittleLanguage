module Typecheck.Typecheck (typecheck) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M

import Utils
import Typecheck.Utils
import qualified Rename.Syntax as Rename
import Typecheck.Syntax
import Codegen.Builtins
import RuntimeSystem



type SymbolTable = M.Map Symbol Type


typecheck :: MonadError Error m => Target -> [Rename.GlobalDefinition] -> m [GlobalDefinition]
typecheck target definitions = evalStateT' M.empty $ do

  mapM_ importGlobalSymbol $ M.toList $ fmap fst builtins

  case target of
    NativeTarget -> mapM_ importGlobalSymbol $ M.toList nativeRuntimeSymbols
    ArduinoTarget _ _ -> mapM_ importGlobalSymbol $ M.toList arduinoRuntimeSymbols
    _ -> return ()

  mapM_ importDefinition definitions

  mapM typecheckGlobalDefinition definitions


importGlobalSymbol :: MonadState SymbolTable m => (GlobalSymbol, Type) -> m ()
importGlobalSymbol (symbol, type_) = do
  let symbol' = SymbolGlobal symbol
  modify $ M.insert symbol' type_


importDefinition :: MonadState SymbolTable m => Rename.GlobalDefinition -> m ()
importDefinition definition = modify $ M.insert (SymbolGlobal $ Rename.globalDefinitionSymbol definition) (Rename.globalDefinitionType definition)


typecheckGlobalDefinition :: (MonadState SymbolTable m, MonadError Error m) => Rename.GlobalDefinition -> m GlobalDefinition
typecheckGlobalDefinition (Rename.GlobalDefinitionValue (Rename.GlobalValueDefinition symbol type_ expression loc)) = do

  (typedExpression, expressionType) <- typecheckExpression expression

  when (type_ /= expressionType) $ throwError ("definition of \"" ++ show symbol ++ "\": expected " ++ show type_ ++ " but got " ++ show expressionType, phase, Just loc)

  return $ GlobalDefinitionValue $ GlobalValueDefinition symbol type_ typedExpression loc

typecheckGlobalDefinition (Rename.GlobalDefinitionFunction (Rename.FunctionDefinition symbol parameters resultType expression loc)) = do

  mapM_ addParameter parameters

  (typedExpression, expressionType) <- typecheckExpression expression

  when (resultType /= expressionType) $ throwError ("definition of \"" ++ show symbol ++ "\": expected " ++ show resultType ++ " but got " ++ show expressionType, phase, Just loc)

  return $ GlobalDefinitionFunction $ FunctionDefinition symbol parameters resultType typedExpression loc


addParameter :: MonadState SymbolTable m => Parameter -> m ()
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

  when (type_ /= expressionType) $ throwError ("definition of \"" ++ show symbol ++ "\": expected " ++ show type_ ++ " but got " ++ show expressionType, phase, Just loc)

  return $ LocalValueDefinition symbol type_ typedExpression loc


typecheckExpression :: (MonadState SymbolTable m, MonadError Error m) => Rename.Expression -> m (Expression, Type)
typecheckExpression (Rename.Unit loc) = return (Unit TypeUnit loc, TypeUnit)

typecheckExpression (Rename.LiteralExpression value loc) = do
    let type_ = literalType value
    return (LiteralExpression value type_ loc, type_)

typecheckExpression (Rename.SymbolReference symbol loc) = do

  symbolType <- findSymbol symbol

  case symbolType of
    Just symbolType' -> return (SymbolReference symbol symbolType' loc, symbolType')
    Nothing -> throwError ("symbol \"" ++ show symbol ++ "\" not found", phase, Just loc)

typecheckExpression (Rename.Call symbol arguments loc) = do

  symbolType <- findSymbol symbol

  (parameterTypes, resultType) <- case symbolType of
    Just (TypeFunction parameterTypes' resultType') -> return (parameterTypes', resultType')
    Just _ -> throwError ("can't call non-function symbol: " ++ show symbol, phase, Just loc)
    Nothing -> throwError ("function not found: " ++ show symbol, phase, Just loc)

  (typedArguments, argumentTypes) <- mapAndUnzipM typecheckExpression arguments

  let numberArgumentsExpected = length parameterTypes
  let numberArgumentsPassed = length argumentTypes
  when (numberArgumentsExpected /= numberArgumentsPassed) $ throwError ("call of function " ++ show symbol ++ ": wrong number of arguments. expected: " ++ show numberArgumentsExpected ++ ", passed: " ++ show numberArgumentsPassed, phase, Just loc)

  mapM_ typecheckParameter $ zip parameterTypes argumentTypes

  return (Call symbol typedArguments resultType loc, resultType) where

    typecheckParameter :: (MonadState SymbolTable m, MonadError Error m) => (Type, Type) -> m ()
    typecheckParameter (parameterType, argumentType) = when (parameterType /= argumentType) $ throwError ("call of function" ++ show symbol ++ ": expected " ++ show parameterType ++ " but got " ++ show argumentType, phase, Just loc)

typecheckExpression (Rename.If conditionExpression trueExpression falseExpression loc) = do

  (typedConditionExpression, conditionExpressionType) <- typecheckExpression conditionExpression

  when (conditionExpressionType /= TypeBoolean) $ throwError ("condition expression must be of type " ++ show TypeBoolean ++ " but has type " ++ show conditionExpressionType, phase, Just loc)

  (typedTrueExpression, trueExpressionType) <- typecheckExpression trueExpression
  (typedFalseExpression, falseExpressionType) <- typecheckExpression falseExpression

  when (trueExpressionType /= falseExpressionType) $ throwError ("types of if branches differ: " ++ show trueExpressionType ++ " and " ++ show falseExpressionType, phase, Just loc)

  return (If typedConditionExpression typedTrueExpression typedFalseExpression trueExpressionType loc, trueExpressionType)


typecheckExpression (Rename.Do statements loc) = do

  (typedStatements, statementTypes) <- mapAndUnzipM typecheckStatement statements

  case lastMaybe statementTypes of
   Just lastStatementType -> return (Do typedStatements lastStatementType loc, lastStatementType)
   Nothing -> throwError ("empty do block", phase, Just loc)


literalType :: Literal -> Type
literalType (Pointer _) = TypePointer
literalType (Int _) = TypeInt
literalType (Int8 _) = TypeInt8
literalType (Float _) = TypeFloat


findSymbol :: MonadState SymbolTable m => Symbol -> m (Maybe Type)
findSymbol symbol = gets $ M.lookup symbol
