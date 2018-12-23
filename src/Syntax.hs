module Syntax where


type Name = String

data Type
  = TypeUnit | TypeBoolean | TypeInt | TypeFloat
  deriving (Eq, Ord, Show)

data VarDecl
  = VarDecl Loc Name Type
  deriving (Eq, Ord, Show)

data FuncSignature
  = FuncSignature Type [Type]
  deriving (Eq, Ord, Show)

data FuncDecl
  = FuncDecl Name FuncSignature
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

defToFuncDecl :: Def -> FuncDecl
defToFuncDecl (Function _ name fType params _) =
  let params' = map (\(_, paramType) -> paramType) params in
  let signature = FuncSignature fType params' in
  FuncDecl name signature

