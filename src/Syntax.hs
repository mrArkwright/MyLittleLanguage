module Syntax where


type Name = String

data Type
  = TypeInt | TypeFloat
  deriving (Eq, Ord, Show)

data Def
  = Function Name [Name] Expr
  deriving (Eq, Ord, Show)

data Statement
  = Expr Expr
  | Let Name Expr
  deriving (Eq, Ord, Show)

data Expr
  = Float Double
  | Int Integer
  | Var Name
  | If Expr Expr Expr
  | Call Name [Expr]
  | Do [Statement]
  deriving (Eq, Ord, Show)

