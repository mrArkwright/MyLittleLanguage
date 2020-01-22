module RuntimeSystem where

import qualified Data.Map as M

import Parse.Syntax (Type(..))
import Rename.Syntax



libraryBuiltins :: M.Map GlobalSymbol Type
libraryBuiltins =  M.fromList $ map (\(name, type_) -> (GlobalSymbol name [], type_)) [
    ("exitSuccess", TypeFunction [] TypeUnit),
    ("exitFailure", TypeFunction [] TypeUnit),
    ("printInt", TypeFunction [TypeInt] TypeUnit),
    ("printChar", TypeFunction [TypeInt] TypeUnit),
    ("printFloat", TypeFunction [TypeFloat] TypeUnit),
    ("sin", TypeFunction [TypeFloat] TypeFloat),
    ("sqrt", TypeFunction [TypeFloat] TypeFloat)
  ]
