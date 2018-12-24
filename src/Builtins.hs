module Builtins where

import Syntax


builtins :: [FuncDecl]
builtins = [
    FuncDecl "+" (FuncSignature TypeInt [TypeInt, TypeInt]),
    FuncDecl "-" (FuncSignature TypeInt [TypeInt, TypeInt]),
    FuncDecl "<" (FuncSignature TypeBoolean [TypeInt, TypeInt]),
    FuncDecl "+." (FuncSignature TypeFloat [TypeFloat, TypeFloat]),
    FuncDecl "-." (FuncSignature TypeFloat [TypeFloat, TypeFloat]),
    FuncDecl "*." (FuncSignature TypeFloat [TypeFloat, TypeFloat]),
    FuncDecl "/." (FuncSignature TypeFloat [TypeFloat, TypeFloat]),
    FuncDecl "<." (FuncSignature TypeBoolean [TypeFloat, TypeFloat])
  ]

libraryBuiltins :: [FuncDecl]
libraryBuiltins = [
    FuncDecl "exitSuccess" (FuncSignature TypeUnit []),
    FuncDecl "exitFailure" (FuncSignature TypeUnit []),
    FuncDecl "printInt" (FuncSignature TypeUnit [TypeInt]),
    FuncDecl "printChar" (FuncSignature TypeUnit [TypeInt]),
    FuncDecl "printFloat" (FuncSignature TypeUnit [TypeFloat]),
    FuncDecl "sin" (FuncSignature TypeFloat [TypeFloat]),
    FuncDecl "sqrt" (FuncSignature TypeFloat [TypeFloat])
  ]

