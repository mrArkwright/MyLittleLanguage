module Rename.Rename (rename) where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.MultiMap as MM

import Misc
import Parse.Syntax (Name, SymbolPath, Parameter(..), Type(..))
import qualified Parse.Syntax as Parse
import Rename.Syntax
import Builtins



type SymbolTable = MM.MultiMap Parse.Symbol Symbol



rename :: MonadError Error m => Parse.Module -> m [GlobalDefinition]
rename module_ = evalStateT' MM.empty $ do
  mapM_ importBuiltin (builtins ++ libraryBuiltins)
  importModuleVerbatim module_
  renameModule module_


importBuiltin :: (MonadState SymbolTable m, MonadError Error m) => (Name, Type) -> m ()
importBuiltin (name, _) = modify $ MM.insert (Parse.Symbol name []) (SymbolGlobal $ GlobalSymbol name [])


importModuleVerbatim :: (MonadState SymbolTable m, MonadError Error m) => Parse.Module -> m ()
importModuleVerbatim = importModuleVerbatim' [] where

  importModuleVerbatim' :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> Parse.Module -> m ()
  importModuleVerbatim' modulePath (Parse.Module moduleName submodules definitions) = do

    let modulePath' = modulePath -:+ moduleName

    mapM_ (importDefinition modulePath' modulePath') definitions
    mapM_ (importModuleVerbatim' modulePath') submodules


importSubmodule :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> SymbolPath -> Parse.Module -> m ()
importSubmodule importPath modulePath (Parse.Module moduleName submodules definitions) = do

  let modulePath' = modulePath -:+ moduleName
  let importPath' = importPath -:+ moduleName

  mapM_ (importDefinition importPath' modulePath') definitions
  mapM_ (importSubmodule importPath' modulePath') submodules


importDefinition :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> SymbolPath -> Parse.Definition -> m ()
importDefinition importPath symbolPath definition = do

  let name = Parse.definition_name definition
  let importedSymbol = Parse.Symbol name importPath
  let symbol = SymbolGlobal $ GlobalSymbol name symbolPath

  symbolTable <- get
  when (MM.member symbolTable importedSymbol) $ throwError ("function \"" ++ name ++ "\" redefined", Just $ Parse.definition_loc definition)

  modify $ MM.insert importedSymbol symbol


renameModule :: (MonadState SymbolTable m, MonadError Error m) => Parse.Module -> m [GlobalDefinition]
renameModule = renameModule' [] where

  renameModule' :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> Parse.Module -> m [GlobalDefinition]
  renameModule' modulePath (Parse.Module moduleName submodules definitions) = do

    symbolTableBefore <- get

    let modulePath' = modulePath -:+ moduleName

    mapM_ (importDefinition [] modulePath') definitions
    mapM_ (importSubmodule [] modulePath') submodules

    renamedDefinitions <- mapM (renameGlobalDefinition modulePath') definitions
    renamedSubmodules <- mapM (renameModule' modulePath') submodules
    let renamedSubmodulesDefinitions = concat renamedSubmodules

    put symbolTableBefore

    return $ renamedDefinitions ++ renamedSubmodulesDefinitions


renameGlobalDefinition :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> Parse.Definition -> m GlobalDefinition
renameGlobalDefinition symbolPath (Parse.Definition name Nothing returnType expression loc) = do

  let symbol = GlobalSymbol name symbolPath

  expression' <- renameExpression expression

  return $ GlobalDefinitionValue $ GlobalValueDefinition symbol returnType expression' loc

renameGlobalDefinition symbolPath (Parse.Definition name (Just parameters) resultType expression loc) = do

  let symbol = GlobalSymbol name symbolPath

  symbolTableBefore <- get

  mapM_ addParameter parameters

  expression' <- renameExpression expression

  put symbolTableBefore

  return $ GlobalDefinitionFunction $ FunctionDefinition symbol parameters resultType expression' loc


addParameter :: (MonadState SymbolTable m, MonadError Error m) => Parameter -> m ()
addParameter parameter = do
  let name = parameter_name parameter
  let importedSymbol = Parse.Symbol name []
  let symbol = SymbolLocal $ LocalSymbol name
  modify $ MM.insert importedSymbol symbol


renameExpression :: (MonadState SymbolTable m, MonadError Error m) => Parse.Expression -> m Expression
renameExpression (Parse.Unit loc) = return $ Unit loc

renameExpression (Parse.Int value loc) = return $ Int value loc

renameExpression (Parse.Float value loc) = return $ Float value loc

renameExpression (Parse.SymbolReference symbol loc) = do

  symbolTable <- get

  resolvedSymbol <- case MM.lookup symbol symbolTable of
    [] -> throwError ("symbol " ++ show symbol ++ " not found", Just loc)
    [symbol'] -> return symbol'
    _ -> throwError ("ambigous reference to " ++ show symbol, Just loc)

  return $ SymbolReference resolvedSymbol loc

renameExpression (Parse.Call symbol argExprs loc) = do

  argExprs' <- mapM renameExpression argExprs

  symbolTable <- get

  resolvedSymbol <- case MM.lookup symbol symbolTable of
    [] -> throwError ("function " ++ show symbol ++ " not found", Just loc)
    [symbol'] -> return symbol'
    _ -> throwError ("ambigous call of " ++ show symbol, Just loc)

  return $ Call resolvedSymbol argExprs' loc

renameExpression (Parse.If condExpr trueExpr falseExpr loc) = do

  condExpr' <- renameExpression condExpr
  trueExpr' <- renameExpression trueExpr
  falseExpr' <- renameExpression falseExpr

  return $ If condExpr' trueExpr' falseExpr' loc

renameExpression (Parse.Do statements loc) = do
  statements' <- mapM renameStatement statements
  return $ Do statements' loc



renameStatement :: (MonadState SymbolTable m, MonadError Error m) => Parse.Statement -> m Statement
renameStatement (Parse.StatementExpression expression loc) = do
  expression' <- renameExpression expression
  return $ StatementExpression expression' loc

renameStatement (Parse.StatementDefinition (Parse.Definition name Nothing resultType expression loc) statementLoc) = do

  let symbol = LocalSymbol name
  modify $ MM.insert (Parse.Symbol name []) (SymbolLocal symbol)

  expression' <- renameExpression expression

  return $ StatementDefinition (LocalValueDefinition symbol resultType expression' loc) statementLoc

renameStatement (Parse.StatementDefinition definition@(Parse.Definition name (Just parameters) resultType _ loc) statementLoc) = do

  definition' <- renameGlobalDefinition [] definition
  -- TODO correct path, import definition, add definition' to AST

  let symbol = LocalSymbol name
  let type_ = TypeFunction (map parameter_type parameters) resultType
  let referenceExpression = SymbolReference (SymbolGlobal $ globalDefinitionSymbol definition') loc

  return $ StatementDefinition (LocalValueDefinition symbol type_ referenceExpression loc) statementLoc
