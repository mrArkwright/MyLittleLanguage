module Rename (rename) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Morph

import Data.List
import qualified Data.MultiMap as MM

import Misc
import Syntax
import Builtins


type SymbolTable = MM.MultiMap Symbol Symbol

type Rename = StateT SymbolTable (Except Error)



rename :: Monad m => Module () -> ExceptT Error m [Def ()]
rename module_ = hoist generalize $ evalStateT' MM.empty $ do
  mapM_ importBuiltin (builtins ++ libraryBuiltins)
  importModuleVerbatim module_
  renameModule module_


importBuiltin :: FuncDecl -> Rename ()
importBuiltin (FuncDecl name _) = do
  let symbol = Symbol name []
  modify $ MM.insert symbol symbol


importModuleVerbatim :: Module () -> Rename ()
importModuleVerbatim = importModuleVerbatim' [] where

  importModuleVerbatim' :: SymbolPath -> Module () -> Rename ()
  importModuleVerbatim' modulePath (Module moduleName submodules definitions) = do

    let modulePath' = modulePath ++ [moduleName]

    mapM_ (importDefinition modulePath' modulePath') definitions
    mapM_ (importModuleVerbatim' modulePath') submodules


importSubmodule :: SymbolPath -> SymbolPath -> Module () -> Rename ()
importSubmodule importPath modulePath (Module moduleName submodules definitions) = do

  let modulePath' = modulePath ++ [moduleName]
  let importPath' = importPath ++ [moduleName]

  mapM_ (importDefinition importPath' modulePath') definitions
  mapM_ (importSubmodule importPath' modulePath') submodules


importDefinition :: SymbolPath -> SymbolPath -> Def () -> Rename ()
importDefinition importPath symbolPath definition = do

  let (FuncDecl name _) = defToFuncDecl definition
  let symbol = Symbol name symbolPath
  let importedSymbol = Symbol name importPath

  symbolTable <- get
  when (MM.member symbolTable importedSymbol) $ throwError ("function \"" ++ name ++ "\" redefined", Just $ locFromDef definition)

  modify $ MM.insert importedSymbol symbol


renameModule :: Module () -> Rename [Def ()]
renameModule = renameModule' [] where

  renameModule' :: SymbolPath -> Module () -> Rename [Def ()]
  renameModule' modulePath (Module moduleName submodules definitions) = do

    symbolTableBefore <- get

    let modulePath' = modulePath ++ [moduleName]

    mapM_ (importDefinition [] modulePath') definitions
    mapM_ (importSubmodule [] modulePath') submodules

    renamedDefinitions <- mapM (renameDefinition modulePath') definitions
    renamedSubmodules <- mapM (renameModule' modulePath') submodules
    let renamedSubmodulesDefinitions = concat renamedSubmodules

    put symbolTableBefore

    return $ renamedDefinitions ++ renamedSubmodulesDefinitions


renameDefinition :: SymbolPath -> Def () -> Rename (Def ())
renameDefinition modulePath (Function fName fType fArgs fExpr fLoc) = do

  let fName' = intercalate "." $ modulePath ++ [fName]

  fExpr' <- renameExpr fExpr

  return $ Function fName' fType fArgs fExpr' fLoc


renameExpr :: Expr () -> Rename (Expr ())
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


renameStatement :: Statement () -> Rename (Statement ())
renameStatement (Expr expr tag loc) = do
  expr' <- renameExpr expr
  return $ Expr expr' tag loc

renameStatement (Let name type_ expr tag loc) = do
  expr' <- renameExpr expr
  return $ Let name type_ expr' tag loc

