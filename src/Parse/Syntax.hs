module Parse.Syntax where

import Data.List

import Misc


type Name = String

type SymbolPath = [Name]

data Symbol = Symbol {
    symbol_name :: Name,
    symbol_path :: SymbolPath
  } deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol name path) = intercalate "." $ path -:+ name


data Type
  = TypeUnit | TypePointer | TypeBoolean | TypeInt | TypeInt8 | TypeFloat | TypeFunction [Type] Type
  deriving (Eq, Ord, Show)


data Module = Module {
    module_name :: Name,
    module_submodules :: [Module],
    module_definitions :: [Definition]
  } deriving (Eq, Ord, Show)


data Parameter = Parameter {
    parameter_name :: Name,
    parameter_type :: Type
  } deriving (Eq, Ord, Show)


data Definition = Definition {
    definition_name :: Name,
    definition_parameters :: Maybe [Parameter],
    definition_resultType :: Type,
    definition_expression :: Expression,
    definition_loc :: Loc
  } deriving (Eq, Ord, Show)


data Statement
  = StatementExpression Expression Loc
  | StatementDefinition Definition Loc
  deriving (Eq, Ord, Show)


data Expression
  = Unit Loc
  | Pointer Integer Loc
  | Int Integer Loc
  | Int8 Integer Loc
  | Float Double Loc
  | SymbolReference Symbol Loc
  | Call Symbol [Expression] Loc
  | If Expression Expression Expression Loc
  | Do [Statement] Loc
  deriving (Eq, Ord, Show)
