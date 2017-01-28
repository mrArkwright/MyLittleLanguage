module Syntax where

type Name = String

data Def
  = Function Name [Name] Expr
  | Extern Name [Name]
  deriving (Eq, Ord, Show)

data Statement
  = Expr Expr
  | Let Name Expr
  deriving (Eq, Ord, Show)

data Expr
  = Float Double
  | Var Name
  | If Expr Expr Expr
  | Call Name [Expr]
  | Do [Statement]
  deriving (Eq, Ord, Show)

