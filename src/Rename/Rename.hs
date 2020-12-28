module Rename.Rename (rename) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.MultiMap as MM

import Misc
import qualified Parse.Syntax as Parse
import Rename.Syntax
import Codegen.Builtins
import RuntimeSystem



rename :: MonadError Error m => Parse.Module -> m [GlobalDefinition]
rename module_ = evalStateT' (Rename MM.empty []) $ do

  mapM_ importBuiltin $ M.toList $ fmap (\(type_, _) -> type_) builtins
  mapM_ importBuiltin $ M.toList libraryBuiltins
  importModuleVerbatim module_
  
  renameModule module_
  
  gets rename_definitions


importBuiltin :: (MonadState Rename m, MonadError Error m) => (GlobalSymbol, Type) -> m ()
importBuiltin (symbol @ (GlobalSymbol name symbolPath), _) = addToSymbolTable (Parse.Symbol name symbolPath) (SymbolGlobal symbol)


importModuleVerbatim :: (MonadState Rename m, MonadError Error m) => Parse.Module -> m ()
importModuleVerbatim = importModuleVerbatim' [] where

  importModuleVerbatim' :: (MonadState Rename m, MonadError Error m) => SymbolPath -> Parse.Module -> m ()
  importModuleVerbatim' modulePath (Parse.Module moduleName submodules definitions) = do

    let modulePath' = modulePath -:+ moduleName

    mapM_ (importDefinition modulePath' modulePath') definitions
    mapM_ (importModuleVerbatim' modulePath') submodules


importSubmodule :: (MonadState Rename m, MonadError Error m) => SymbolPath -> SymbolPath -> Parse.Module -> m ()
importSubmodule importPath modulePath (Parse.Module moduleName submodules definitions) = do

  let modulePath' = modulePath -:+ moduleName
  let importPath' = importPath -:+ moduleName

  mapM_ (importDefinition importPath' modulePath') definitions
  mapM_ (importSubmodule importPath' modulePath') submodules


importDefinition :: (MonadState Rename m, MonadError Error m) => SymbolPath -> SymbolPath -> Parse.Definition -> m ()
importDefinition importPath symbolPath definition = do

  let name = Parse.definition_name definition
  let importedSymbol = Parse.Symbol name importPath
  let symbol = SymbolGlobal $ GlobalSymbol name symbolPath

  symbolTable <- gets rename_symbolTable
  when (MM.member symbolTable importedSymbol) $ throwError ("(Rename) function \"" ++ name ++ "\" redefined", Just $ Parse.definition_loc definition)

  modify $ \s -> s { rename_symbolTable = MM.insert importedSymbol symbol symbolTable }


renameModule :: (MonadState Rename m, MonadError Error m) => Parse.Module -> m ()
renameModule = renameModule' [] where

  renameModule' :: (MonadState Rename m, MonadError Error m) => SymbolPath -> Parse.Module -> m ()
  renameModule' modulePath (Parse.Module moduleName submodules definitions) = do

    symbolTableBefore <- gets rename_symbolTable

    let modulePath' = modulePath -:+ moduleName

    mapM_ (importDefinition [] modulePath') definitions
    mapM_ (importSubmodule [] modulePath') submodules

    mapM_ (renameGlobalDefinition modulePath') definitions
    mapM_ (renameModule' modulePath') submodules

    modify $ \s -> s { rename_symbolTable = symbolTableBefore }


renameGlobalDefinition :: (MonadState Rename m, MonadError Error m) => SymbolPath -> Parse.Definition -> m GlobalSymbol
renameGlobalDefinition symbolPath (Parse.Definition name Nothing returnType expression loc) = do

  let symbol = GlobalSymbol name symbolPath

  let symbolPath' = symbolPath -:+ name
  expression' <- runReaderT' symbolPath' $ renameExpression expression

  addToDefinitions $ GlobalDefinitionValue $ GlobalValueDefinition symbol returnType expression' loc

  return symbol

renameGlobalDefinition symbolPath (Parse.Definition name (Just parameters) resultType expression loc) = do

  let symbol = GlobalSymbol name symbolPath

  symbolTableBefore <- gets rename_symbolTable

  mapM_ addParameter parameters

  let symbolPath' = symbolPath -:+ name
  expression' <- runReaderT' symbolPath' $ renameExpression expression

  modify $ \s -> s { rename_symbolTable = symbolTableBefore }

  addToDefinitions $ GlobalDefinitionFunction $ FunctionDefinition symbol parameters resultType expression' loc

  return symbol


addParameter :: (MonadState Rename m, MonadError Error m) => Parameter -> m ()
addParameter parameter = do
  let name = parameter_name parameter
  let importedSymbol = Parse.Symbol name []
  let symbol = SymbolLocal $ LocalSymbol name
  addToSymbolTable importedSymbol symbol


renameExpression :: (MonadReader SymbolPath m, MonadState Rename m, MonadError Error m) => Parse.Expression -> m Expression
renameExpression (Parse.Unit loc) = return $ Unit loc

renameExpression (Parse.Pointer value loc) = return $ Pointer value loc

renameExpression (Parse.Int value loc) = return $ Int value loc

renameExpression (Parse.Float value loc) = return $ Float value loc

renameExpression (Parse.SymbolReference symbol loc) = do

  symbolTable <- gets rename_symbolTable

  resolvedSymbol <- case MM.lookup symbol symbolTable of
    [] -> throwError ("(Rename) symbol " ++ show symbol ++ " not found", Just loc)
    [symbol'] -> return symbol'
    _ -> throwError ("(Rename) ambigous reference to " ++ show symbol, Just loc)

  return $ SymbolReference resolvedSymbol loc

renameExpression (Parse.Call symbol argExprs loc) = do

  argExprs' <- mapM renameExpression argExprs

  symbolTable <- gets rename_symbolTable

  resolvedSymbol <- case MM.lookup symbol symbolTable of
    [] -> throwError ("(Rename) function " ++ show symbol ++ " not found", Just loc)
    [symbol'] -> return symbol'
    _ -> throwError ("(Rename) ambigous call of " ++ show symbol, Just loc)

  return $ Call resolvedSymbol argExprs' loc

renameExpression (Parse.If condExpr trueExpr falseExpr loc) = do

  condExpr' <- renameExpression condExpr
  trueExpr' <- renameExpression trueExpr
  falseExpr' <- renameExpression falseExpr

  return $ If condExpr' trueExpr' falseExpr' loc

renameExpression (Parse.Do statements loc) = do
  statements' <- mapM renameStatement statements
  return $ Do statements' loc



renameStatement :: (MonadReader SymbolPath m, MonadState Rename m, MonadError Error m) => Parse.Statement -> m Statement
renameStatement (Parse.StatementExpression expression loc) = do
  expression' <- renameExpression expression
  return $ StatementExpression expression' loc

renameStatement (Parse.StatementDefinition (Parse.Definition name Nothing resultType expression loc) statementLoc) = do

  let symbol = LocalSymbol name
  addToSymbolTable (Parse.Symbol name []) (SymbolLocal symbol)

  expression' <- renameExpression expression

  return $ StatementDefinition (LocalValueDefinition symbol resultType expression' loc) statementLoc

renameStatement (Parse.StatementDefinition definition@(Parse.Definition name (Just parameters) resultType _ loc) statementLoc) = do

  symbolPath <- ask
  definitionSymbol <- renameGlobalDefinition symbolPath definition

  let symbol = LocalSymbol name
  addToSymbolTable (Parse.Symbol name []) (SymbolLocal symbol)

  let type_ = TypeFunction (map parameter_type parameters) resultType
  let referenceExpression = SymbolReference (SymbolGlobal definitionSymbol) loc

  return $ StatementDefinition (LocalValueDefinition symbol type_ referenceExpression loc) statementLoc



--------------------------------------------------------------------------------
-- Primitives and Data Definitions
--------------------------------------------------------------------------------


addToSymbolTable :: (MonadState Rename m, MonadError Error m) => Parse.Symbol -> Symbol -> m ()
addToSymbolTable symbol importedSymbol = do
  symbolTable <- gets rename_symbolTable
  modify $ \s -> s { rename_symbolTable = MM.insert symbol importedSymbol symbolTable }


addToDefinitions :: (MonadState Rename m, MonadError Error m) => GlobalDefinition -> m ()
addToDefinitions definition = do
  definitions <- gets rename_definitions
  modify $ \s -> s { rename_definitions = definitions -:+ definition }


data Rename = Rename {
  rename_symbolTable :: SymbolTable,
  rename_definitions :: [GlobalDefinition]
}


type SymbolTable = MM.MultiMap Parse.Symbol Symbol
