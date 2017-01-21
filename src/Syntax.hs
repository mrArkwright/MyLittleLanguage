module Syntax where

type Name = String

data Def
  = Extern Name [Name]
  | Function Name [Name] Expr
  deriving (Eq, Ord, Show)

data Expr
  = Float Double
  | Var Name
  | Call Name [Expr]
  | BinOp Op Expr Expr
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
