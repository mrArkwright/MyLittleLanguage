module Syntax where


type Name = String

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

data Def
  = Function Name Type [(Name, Type)] Expr Loc
  deriving (Eq, Ord, Show)

data Statement
  = Expr Expr Loc
  | Let Name Type Expr Loc
  deriving (Eq, Ord, Show)

data Expr
  = Unit Loc
  | Int Integer Loc
  | Float Double Loc
  | Var Name Loc
  | If Expr Expr Expr Loc
  | Call Name [Expr] Loc
  | Do [Statement] Loc
  deriving (Eq, Ord, Show)

data Loc = LineLocation Int
  deriving (Eq, Ord, Show)

locDescription :: Loc -> String
locDescription (LineLocation i) = "line " ++ show i ++ ": "

locFromDef :: Def -> Loc
locFromDef (Function _ _ _ _ loc) = loc

defToFuncDecl :: Def -> FuncDecl
defToFuncDecl (Function name fType params _ _) =
  let params' = map (\(_, paramType) -> paramType) params in
  let signature = FuncSignature fType params' in
  FuncDecl name signature

