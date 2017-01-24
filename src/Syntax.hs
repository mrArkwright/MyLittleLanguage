module Syntax where

type Name = String

data Def
  = Function Name [Name] Expr
  | Extern Name [Name]
  deriving (Eq, Ord, Show)

data Expr
  = Float Double
  | Var Name
  | Call Name [Expr]
  deriving (Eq, Ord, Show)

