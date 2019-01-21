module Lex (
    parseInteger,
    parseFloat,
    parseParens,
    parseCommaSep,
    parseSemiSep,
    parseIdentifier,
    parseReserved,
    parseReservedOperator,
    parseDot,
    parseWhiteSpace
  ) where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok


parseInteger :: Parser Integer
parseInteger = Tok.integer lexer

parseFloat :: Parser Double
parseFloat = Tok.float lexer

parseParens :: Parser a -> Parser a
parseParens = Tok.parens lexer

parseCommaSep :: Parser a -> Parser [a]
parseCommaSep = Tok.commaSep lexer

parseSemiSep :: Parser a -> Parser [a]
parseSemiSep = Tok.semiSep lexer

parseIdentifier :: Parser String
parseIdentifier = Tok.identifier lexer

parseReserved :: String -> Parser ()
parseReserved = Tok.reserved lexer

parseReservedOperator :: String -> Parser ()
parseReservedOperator = Tok.reservedOp lexer

parseDot :: Parser ()
parseDot = do
  _ <- Tok.dot lexer
  return ()

parseWhiteSpace :: Parser ()
parseWhiteSpace = Tok.whiteSpace lexer


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef where
  reservedNames = ["module", "def", "do", "end", "let", "if", "then", "else", "Unit", "Int", "Float"]
  reservedOpNames = ["+", "-", "<", "+.", "-.", "*.", "/.", "<.", "="]
  langDef = emptyDef {
    Tok.commentLine = "//",
    Tok.commentStart = "/*",
    Tok.commentEnd = "*/",
    Tok.reservedNames = reservedNames,
    Tok.reservedOpNames = reservedOpNames
  }

