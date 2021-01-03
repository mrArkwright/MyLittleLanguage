module RuntimeSystem where

import qualified Data.Map as M

import Typecheck.Syntax



nativeBuiltins :: M.Map GlobalSymbol Type
nativeBuiltins =  M.fromList $ map (\(name, type_) -> (GlobalSymbol name [], type_)) [
    ("exitSuccess", TypeFunction [] TypeUnit),
    ("exitFailure", TypeFunction [] TypeUnit),
    ("printInt", TypeFunction [TypeInt] TypeUnit),
    ("printChar", TypeFunction [TypeInt] TypeUnit),
    ("printFloat", TypeFunction [TypeFloat] TypeUnit),
    ("sin", TypeFunction [TypeFloat] TypeFloat),
    ("sqrt", TypeFunction [TypeFloat] TypeFloat)
  ]


arduinoBuiltins :: M.Map GlobalSymbol Type
arduinoBuiltins =  M.fromList $ map (\(name, type_) -> (GlobalSymbol name [], type_)) [
    ("digitalWrite", TypeFunction [TypeInt8, TypeInt8] TypeUnit),
    ("delay", TypeFunction [TypeInt] TypeUnit)
  ]
