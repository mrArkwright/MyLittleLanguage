module Rename.Syntax (module Parse.Syntax, module Rename.Syntax) where

import Data.List

import Misc
import Parse.Syntax (Name, SymbolPath, Type(..), Parameter(..))


data GlobalSymbol = GlobalSymbol {
    globalSymbol_name :: Name,
    globalSymbol_path :: SymbolPath
  } deriving (Eq, Ord)

instance Show GlobalSymbol where
  show symbol = intercalate "." $ (globalSymbol_path symbol) -:+ (globalSymbol_name symbol)


data LocalSymbol = LocalSymbol {
    localSymbol_name :: Name
  } deriving (Eq, Ord)

instance Show LocalSymbol where
  show symbol = localSymbol_name symbol


data Symbol = SymbolGlobal GlobalSymbol | SymbolLocal LocalSymbol
  deriving (Eq, Ord)

instance Show Symbol where
  show (SymbolGlobal symbol) = show symbol
  show (SymbolLocal symbol) = show symbol


data LocalValueDefinition = LocalValueDefinition {
    localValueDefinition_symbol :: LocalSymbol,
    localValueDefinition_type :: Type,
    localValueDefinition_expression :: Expression,
    localValueDefinition_loc :: Loc
  } deriving (Eq, Ord, Show)


data GlobalValueDefinition = GlobalValueDefinition {
    globalValueDefinition_symbol :: GlobalSymbol,
    globalValueDefinition_type :: Type,
    globalValueDefinition_expression :: Expression,
    globalValueDefinition_loc :: Loc
  } deriving (Eq, Ord, Show)


data FunctionDefinition = FunctionDefinition {
    functionDefinition_symbol :: GlobalSymbol,
    functionDefinition_parameters :: [Parameter],
    functionDefinition_resultType :: Type,
    functionDefinition_expression :: Expression,
    functionDefinition_loc :: Loc
  } deriving (Eq, Ord, Show)


data GlobalDefinition = GlobalDefinitionValue GlobalValueDefinition | GlobalDefinitionFunction FunctionDefinition
  deriving (Eq, Ord, Show)

globalDefinitionSymbol :: GlobalDefinition -> GlobalSymbol
globalDefinitionSymbol (GlobalDefinitionValue definition) = globalValueDefinition_symbol definition
globalDefinitionSymbol (GlobalDefinitionFunction definition) = functionDefinition_symbol definition

globalDefinitionType :: GlobalDefinition -> Type
globalDefinitionType (GlobalDefinitionValue definition) = globalValueDefinition_type definition
globalDefinitionType (GlobalDefinitionFunction definition) = TypeFunction (map parameter_type $ functionDefinition_parameters definition) (functionDefinition_resultType definition)


data Statement
  = StatementExpression Expression Loc
  | StatementDefinition LocalValueDefinition Loc
  deriving (Eq, Ord, Show)


data Expression
  = Unit Loc
  | Int Integer Loc
  | Float Double Loc
  | SymbolReference Symbol Loc
  | Call Symbol [Expression] Loc
  | If Expression Expression Expression Loc
  | Do [Statement] Loc
  deriving (Eq, Ord, Show)
