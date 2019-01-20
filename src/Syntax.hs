module Syntax where

import Misc


type Name = String

type SymbolPath = [Name]

data Symbol = Symbol {
    _symbolName :: Name,
    _symbolPath :: SymbolPath
  } deriving (Eq, Ord, Show)

data Type
  = TypeUnit | TypeBoolean | TypeInt | TypeFloat
  deriving (Eq, Ord, Show)

data VarDecl
  = VarDecl Name Type Loc
  deriving (Eq, Ord, Show)

data FuncSignature
  = FuncSignature Type [Type]
  deriving (Eq, Ord, Show)

data FuncDecl
  = FuncDecl Name FuncSignature
  deriving (Eq, Ord, Show)

data Module tag
  = Module Name [Module tag] [Def tag]

data Def tag
  = Function Name Type [(Name, Type)] (Expr tag) Loc
  deriving (Eq, Ord, Show)

data Statement tag
  = Expr (Expr tag) tag Loc
  | Let Name Type (Expr tag) tag Loc
  deriving (Eq, Ord, Show)

data Expr tag
  = Unit tag Loc
  | Int Integer tag Loc
  | Float Double tag Loc
  | Var Name tag Loc
  | If (Expr tag) (Expr tag) (Expr tag) tag Loc
  | Call Symbol [(Expr tag)] tag Loc
  | Do [Statement tag] tag Loc
  deriving (Eq, Ord, Show)

locFromDef :: Def tag -> Loc
locFromDef (Function _ _ _ _ loc) = loc

defToFuncDecl :: Def tag -> FuncDecl
defToFuncDecl (Function name fType params _ _) =
  let params' = map (\(_, paramType) -> paramType) params in
  let signature = FuncSignature fType params' in
  FuncDecl name signature

