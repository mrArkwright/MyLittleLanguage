module Rename (rename) where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.MultiMap as MM

import Misc
import Syntax
import Builtins



type SymbolTable = MM.MultiMap Symbol Symbol



rename :: MonadError Error m => Module () -> m [Def ()]
rename module_ = evalStateT' MM.empty $ do
  mapM_ importBuiltin (builtins ++ libraryBuiltins)
  importModuleVerbatim module_
  renameModule module_


importBuiltin :: (MonadState SymbolTable m, MonadError Error m) => FuncDecl -> m ()
importBuiltin (FuncDecl symbol _) = modify $ MM.insert symbol symbol


importModuleVerbatim :: (MonadState SymbolTable m, MonadError Error m) => Module () -> m ()
importModuleVerbatim = importModuleVerbatim' [] where

  importModuleVerbatim' :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> Module () -> m ()
  importModuleVerbatim' modulePath (Module moduleName submodules definitions) = do

    let modulePath' = modulePath -:+ moduleName

    mapM_ (importDefinition modulePath' modulePath') definitions
    mapM_ (importModuleVerbatim' modulePath') submodules


importSubmodule :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> SymbolPath -> Module () -> m ()
importSubmodule importPath modulePath (Module moduleName submodules definitions) = do

  let modulePath' = modulePath -:+ moduleName
  let importPath' = importPath -:+ moduleName

  mapM_ (importDefinition importPath' modulePath') definitions
  mapM_ (importSubmodule importPath' modulePath') submodules


importDefinition :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> SymbolPath -> Def () -> m ()
importDefinition importPath symbolPath definition = do

  let (FuncDecl (Symbol name _) _) = defToFuncDecl definition
  let symbol = Symbol name symbolPath
  let importedSymbol = Symbol name importPath

  symbolTable <- get
  when (MM.member symbolTable importedSymbol) $ throwError ("function \"" ++ name ++ "\" redefined", Just $ locFromDef definition)

  modify $ MM.insert importedSymbol symbol


renameModule :: (MonadState SymbolTable m, MonadError Error m) => Module () -> m [Def ()]
renameModule = renameModule' [] where

  renameModule' :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> Module () -> m [Def ()]
  renameModule' modulePath (Module moduleName submodules definitions) = do

    symbolTableBefore <- get

    let modulePath' = modulePath -:+ moduleName

    mapM_ (importDefinition [] modulePath') definitions
    mapM_ (importSubmodule [] modulePath') submodules

    renamedDefinitions <- mapM (renameDefinition modulePath') definitions
    renamedSubmodules <- mapM (renameModule' modulePath') submodules
    let renamedSubmodulesDefinitions = concat renamedSubmodules

    put symbolTableBefore

    return $ renamedDefinitions ++ renamedSubmodulesDefinitions


renameDefinition :: (MonadState SymbolTable m, MonadError Error m) => SymbolPath -> Def () -> m (Def ())
renameDefinition symbolPath (Function symbol returnType args expr loc) = do

  let symbol' = Symbol (_symbolName symbol) symbolPath

  expr' <- renameExpr expr

  return $ Function symbol' returnType args expr' loc


renameExpr :: (MonadState SymbolTable m, MonadError Error m) => Expr () -> m (Expr ())
renameExpr (If condExpr trueExpr falseExpr tag loc) = do

  condExpr' <- renameExpr condExpr
  trueExpr' <- renameExpr trueExpr
  falseExpr' <- renameExpr falseExpr

  return $ If condExpr' trueExpr' falseExpr' tag loc

renameExpr (Do statements tag loc) = do
  statements' <- mapM renameStatement statements
  return $ Do statements' tag loc

renameExpr (Call symbol argExprs tag loc) = do

  argExprs' <- mapM renameExpr argExprs

  symbolTable <- get

  resolvedSymbol <- case MM.lookup symbol symbolTable of
    [] -> throwError ("function " ++ show symbol ++ " not found", Just loc)
    [symbol'] -> return symbol'
    _ -> throwError ("ambigous call of " ++ show symbol, Just loc)

  return $ Call resolvedSymbol argExprs' tag loc

renameExpr expr = return expr


renameStatement :: (MonadState SymbolTable m, MonadError Error m) => Statement () -> m (Statement ())
renameStatement (Expr expr tag loc) = do
  expr' <- renameExpr expr
  return $ Expr expr' tag loc

renameStatement (Let name type_ expr tag loc) = do
  expr' <- renameExpr expr
  return $ Let name type_ expr' tag loc

