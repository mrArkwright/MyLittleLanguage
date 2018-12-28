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
  | Call Name [(Expr tag)] tag Loc
  | Do [Statement tag] tag Loc
  deriving (Eq, Ord, Show)

data Loc = LineLocation Int
  deriving (Eq, Ord, Show)

locDescription :: Loc -> String
locDescription (LineLocation i) = "line " ++ show i ++ ": "

locFromDef :: Def tag -> Loc
locFromDef (Function _ _ _ _ loc) = loc

defToFuncDecl :: Def tag -> FuncDecl
defToFuncDecl (Function name fType params _ _) =
  let params' = map (\(_, paramType) -> paramType) params in
  let signature = FuncSignature fType params' in
  FuncDecl name signature

