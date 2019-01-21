module Builtins where

import Syntax


builtins :: [FuncDecl]
builtins = [
    FuncDecl (Symbol "+" []) (FuncSignature TypeInt [TypeInt, TypeInt]),
    FuncDecl (Symbol "-" []) (FuncSignature TypeInt [TypeInt, TypeInt]),
    FuncDecl (Symbol "<" []) (FuncSignature TypeBoolean [TypeInt, TypeInt]),
    FuncDecl (Symbol "+." []) (FuncSignature TypeFloat [TypeFloat, TypeFloat]),
    FuncDecl (Symbol "-." []) (FuncSignature TypeFloat [TypeFloat, TypeFloat]),
    FuncDecl (Symbol "*." []) (FuncSignature TypeFloat [TypeFloat, TypeFloat]),
    FuncDecl (Symbol "/." []) (FuncSignature TypeFloat [TypeFloat, TypeFloat]),
    FuncDecl (Symbol "<." []) (FuncSignature TypeBoolean [TypeFloat, TypeFloat])
  ]

libraryBuiltins :: [FuncDecl]
libraryBuiltins = [
    FuncDecl (Symbol "exitSuccess" []) (FuncSignature TypeUnit []),
    FuncDecl (Symbol "exitFailure" []) (FuncSignature TypeUnit []),
    FuncDecl (Symbol "printInt" []) (FuncSignature TypeUnit [TypeInt]),
    FuncDecl (Symbol "printChar" []) (FuncSignature TypeUnit [TypeInt]),
    FuncDecl (Symbol "printFloat" []) (FuncSignature TypeUnit [TypeFloat]),
    FuncDecl (Symbol "sin" []) (FuncSignature TypeFloat [TypeFloat]),
    FuncDecl (Symbol "sqrt" []) (FuncSignature TypeFloat [TypeFloat])
  ]

