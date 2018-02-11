module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok


integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef where
  ops = ["+", "-", "*", "/", "<", "="]
  names = ["def", "do", "end", "let", ":", "=", "if", "then", "else", "Unit", "Int", "Float", "()"]
  langDef = emptyDef {
    Tok.commentLine = "//",
    Tok.commentStart = "/*",
    Tok.commentEnd = "*/",
    Tok.reservedOpNames = ops,
    Tok.reservedNames = names
  }
