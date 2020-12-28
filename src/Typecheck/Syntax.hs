module Typecheck.Syntax (module Rename.Syntax, module Typecheck.Syntax) where

import Misc
import Rename.Syntax (Name, SymbolPath, Type(..), Parameter(..), Symbol(..), GlobalSymbol(..), LocalSymbol(..))



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
  = StatementExpression Expression Type Loc
  | StatementDefinition LocalValueDefinition Type Loc
  deriving (Eq, Ord, Show)


data Expression
  = Unit Type Loc
  | Pointer Integer Type Loc
  | Int Integer Type Loc
  | Float Double Type Loc
  | SymbolReference Symbol Type Loc
  | Call Symbol [Expression] Type Loc
  | If Expression Expression Expression Type Loc
  | Do [Statement] Type Loc
  deriving (Eq, Ord, Show)
