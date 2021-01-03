module RuntimeSystem where

import qualified Data.Map as M

import Typecheck.Syntax



nativeRuntimeSymbols :: M.Map GlobalSymbol Type
nativeRuntimeSymbols =  M.fromList $ map (\(name, type_) -> (GlobalSymbol name [], type_)) [
    ("exitSuccess", TypeFunction [] TypeUnit),
    ("exitFailure", TypeFunction [] TypeUnit),
    ("printInt", TypeFunction [TypeInt] TypeUnit),
    ("printChar", TypeFunction [TypeInt] TypeUnit),
    ("printFloat", TypeFunction [TypeFloat] TypeUnit),
    ("sin", TypeFunction [TypeFloat] TypeFloat),
    ("sqrt", TypeFunction [TypeFloat] TypeFloat)
  ]


arduinoRuntimeSymbols :: M.Map GlobalSymbol Type
arduinoRuntimeSymbols =  M.fromList $ map (\(name, type_) -> (GlobalSymbol name [], type_)) [
    ("digitalWrite", TypeFunction [TypeInt8, TypeInt8] TypeUnit),
    ("delay", TypeFunction [TypeInt] TypeUnit)
  ]
