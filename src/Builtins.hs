module Builtins where

import Parse.Syntax



builtins :: [(Name, Type)]
builtins = [
    ("+", TypeFunction [TypeInt, TypeInt] TypeInt),
    ("-", TypeFunction [TypeInt, TypeInt] TypeInt),
    ("<", TypeFunction [TypeInt, TypeInt] TypeBoolean),
    ("+.", TypeFunction [TypeFloat, TypeFloat] TypeFloat),
    ("-.", TypeFunction [TypeFloat, TypeFloat] TypeFloat),
    ("*.", TypeFunction [TypeFloat, TypeFloat] TypeFloat),
    ("/.", TypeFunction [TypeFloat, TypeFloat] TypeFloat),
    ("<.", TypeFunction [TypeFloat, TypeFloat] TypeBoolean)
  ]


libraryBuiltins :: [(Name, Type)]
libraryBuiltins = [
    ("exitSuccess", TypeFunction [] TypeUnit),
    ("exitFailure", TypeFunction [] TypeUnit),
    ("printInt", TypeFunction [TypeInt] TypeUnit),
    ("printChar", TypeFunction [TypeInt] TypeUnit),
    ("printFloat", TypeFunction [TypeFloat] TypeUnit),
    ("sin", TypeFunction [TypeFloat] TypeFloat),
    ("sqrt", TypeFunction [TypeFloat] TypeFloat)
  ]
