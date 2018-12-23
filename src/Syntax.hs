module Syntax where


type Name = String

data Type
  = TypeUnit | TypeInt | TypeFloat
  deriving (Eq, Ord, Show)

data Def
  = Function Loc Name Type [(Name, Type)] Expr
  deriving (Eq, Ord, Show)

data Statement
  = Expr Loc Expr
  | Let Loc Name Type Expr
  deriving (Eq, Ord, Show)

data Expr
  = Unit Loc
  | Int Loc Integer
  | Float Loc Double
  | Var Loc Name
  | If Loc Expr Expr Expr
  | Call Loc Name [Expr]
  | Do Loc [Statement]
  deriving (Eq, Ord, Show)

data Loc = LineLocation Int
  deriving (Eq, Ord, Show)

locDescription :: Loc -> String
locDescription (LineLocation i) = "line " ++ show i ++ ": "

locFromDef :: Def -> Loc
locFromDef (Function loc _ _ _ _) = loc
