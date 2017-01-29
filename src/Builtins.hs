module Builtins where


data Builtin = Builtin String [String]

builtins :: [Builtin]
builtins = [
    Builtin "exitSuccess" [],
    Builtin "printChar" ["c"],
    Builtin "printDouble" ["d"],
    Builtin "sin" ["x"]
  ]

