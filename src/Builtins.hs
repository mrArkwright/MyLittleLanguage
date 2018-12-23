module Builtins where


data Builtin = Builtin String [String]

builtins :: [Builtin]
builtins = [
    Builtin "exitSuccess" [],
    Builtin "printInt" ["i"],
    Builtin "printChar" ["c"],
    Builtin "printFloat" ["d"],
    Builtin "sin" ["x"]
  ]

