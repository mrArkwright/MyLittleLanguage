module Typecheck.Syntax (module Rename.Syntax, module Typecheck.Syntax) where

import Utils
import Rename.Syntax (Name, SymbolPath, Type(..), Parameter(..), Symbol(..), GlobalSymbol(..), LocalSymbol(..), Literal(..))



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


data Statement
  = StatementExpression Expression Type Loc
  | StatementDefinition LocalValueDefinition Type Loc
  deriving (Eq, Ord, Show)


data Expression
  = Unit Type Loc
  | LiteralExpression Literal Type Loc
  | SymbolReference Symbol Type Loc
  | Call Symbol [Expression] Type Loc
  | If Expression Expression Expression Type Loc
  | Do [Statement] Type Loc
  deriving (Eq, Ord, Show)
