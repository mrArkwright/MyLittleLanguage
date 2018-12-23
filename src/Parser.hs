module Parser (parse) where

import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Morph

import Data.Functor.Identity

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr

import Misc
import qualified Lexer as L
import Syntax


parse :: Monad m => String -> String -> ExceptT Error m [Def]
parse name source = hoist generalize $ liftEither $ left show $ P.parse myLittleLanguageParser name source

myLittleLanguageParser :: Parser [Def]
myLittleLanguageParser = do
  L.whiteSpace
  definitions <- many definition
  eof
  return definitions


--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

parseType :: Parser Type
parseType = parseUnitType
  <|> parseIntType
  <|> parseFloatType

parseUnitType :: Parser Type
parseUnitType = do
  L.reserved "Unit"
  return TypeUnit

parseIntType :: Parser Type
parseIntType = do
  L.reserved "Int"
  return TypeInt

parseFloatType :: Parser Type
parseFloatType = do
  L.reserved "Float"
  return TypeFloat

--------------------------------------------------------------------------------
-- definitions
--------------------------------------------------------------------------------

definition :: Parser Def
definition = try function

function :: Parser Def
function = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  L.reserved "def"
  name <- L.identifier
  args <- L.parens $ L.commaSep parseArg
  L.reserved ":"
  functionType <- parseType
  L.reserved "="
  body <- expr
  return $ Function loc name functionType args body

parseArg :: Parser (Name, Type)
parseArg = do
  argName <- L.identifier
  L.reserved ":"
  argType <- parseType
  return (argName, argType)



--------------------------------------------------------------------------------
-- expressions
--------------------------------------------------------------------------------

expr :: Parser Expr
expr = buildExpressionParser opTable factor

factor :: Parser Expr
factor = try parseUnit
  <|> try float
  <|> try integer
  <|> try call
  <|> variable
  <|> ifThenElse
  <|> doBlock
  <|> L.parens expr

binary :: String -> Assoc -> Operator String () Identity Expr
binary s assoc = Infix (L.reservedOp s >> return (\x y -> Call (LineLocation 0) s [x, y])) assoc -- TODO location

opTable :: OperatorTable String () Identity Expr
opTable = [
    [binary "*." AssocLeft, binary "/." AssocLeft],
    [binary "+" AssocLeft, binary "-" AssocLeft, binary "+." AssocLeft, binary "-." AssocLeft],
    [binary "<" AssocLeft, binary "<." AssocLeft]
  ]

parseUnit :: Parser Expr
parseUnit = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  L.reserved "()"
  return $ Unit loc

integer :: Parser Expr
integer = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  value <- L.integer
  return $ Int loc value

float :: Parser Expr
float = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  value <- L.float
  return $ Float loc value

variable :: Parser Expr
variable = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  name <- L.identifier
  return $ Var loc name

ifThenElse :: Parser Expr
ifThenElse = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  L.reserved "if"
  condition <- expr
  L.reserved "then"
  ifTrue <- expr
  L.reserved "else"
  ifFalse <- expr
  return $ If loc condition ifTrue ifFalse

call :: Parser Expr
call = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  name <- L.identifier
  args <- L.parens $ L.commaSep expr
  return $ Call loc name args

doBlock :: Parser Expr
doBlock = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  L.reserved "do"
  statements <- many statement
  L.reserved "end"
  return $ Do loc statements



--------------------------------------------------------------------------------
-- statements
--------------------------------------------------------------------------------

statement :: Parser Statement
statement = try expressionStatement
        <|> letStatement

expressionStatement :: Parser Statement
expressionStatement = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  expression <- expr
  return $ Expr loc expression

letStatement :: Parser Statement
letStatement = do
  loc <- LineLocation <$> sourceLine <$> getPosition
  L.reserved "let"
  name <- L.identifier
  L.reserved ":"
  letType <- parseType
  L.reserved "="
  expression <- expr
  return $ Let loc name letType expression

