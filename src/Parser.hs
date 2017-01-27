module Parser where

--import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (buildExpressionParser, Operator(Infix), Assoc(AssocLeft))

import qualified Lexer as L
import Syntax


myLittleLanguageParser :: Parser [Def]
myLittleLanguageParser = do
  L.whiteSpace
  definitions <- many definition
  eof
  return definitions



--------------------------------------------------------------------------------
-- definitions
--------------------------------------------------------------------------------

definition :: Parser Def
definition = try extern
         <|> try function

function :: Parser Def
function = do
  L.reserved "def"
  name <- L.identifier
  args <- L.parens $ many L.identifier
  body <- expr
  return $ Function name args body

extern :: Parser Def
extern = do
  L.reserved "extern"
  name <- L.identifier
  args <- L.parens $ many L.identifier
  return $ Extern name args



--------------------------------------------------------------------------------
-- expressions
--------------------------------------------------------------------------------

expr :: Parser Expr
expr = buildExpressionParser opTable factor

factor :: Parser Expr
factor = try float
     <|> try call
     <|> variable
     <|> ifThenElse
     <|> L.parens expr

binary s assoc = Infix (L.reservedOp s >> return (\x y -> Call s [x, y])) assoc

opTable = [[binary "*" AssocLeft, binary "/" AssocLeft],
         [binary "+" AssocLeft, binary "-" AssocLeft],
         [binary "<" AssocLeft]]

float :: Parser Expr
float = do
  value <- L.float
  return $ Float value

variable :: Parser Expr
variable = do
  name <- L.identifier
  return $ Var name

ifThenElse :: Parser Expr
ifThenElse = do
  L.reserved "if"
  condition <- expr
  L.reserved "then"
  ifTrue <- expr
  L.reserved "else"
  ifFalse <- expr
  return $ If condition ifTrue ifFalse

call :: Parser Expr
call = do
  name <- L.identifier
  args <- L.parens $ L.commaSep expr
  return $ Call name args

