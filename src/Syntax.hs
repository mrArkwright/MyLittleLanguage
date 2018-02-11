module Syntax where


type Name = String

data Type
  = TypeUnit | TypeInt | TypeFloat
  deriving (Eq, Ord, Show)

data Def
  = Function Name Type [(Name, Type)] Expr
  deriving (Eq, Ord, Show)

data Statement
  = Expr Expr
  | Let Name Type Expr
  deriving (Eq, Ord, Show)

data Expr
  = Unit
  | Int Integer
  | Float Double
  | Var Name
  | If Expr Expr Expr
  | Call Name [Expr]
  | Do [Statement]
  deriving (Eq, Ord, Show)

