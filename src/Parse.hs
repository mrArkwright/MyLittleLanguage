module Parse (parse) where

import Control.Arrow (left)
import Control.Monad.Except
import Control.Monad.Morph

import Data.Functor.Identity
import Data.Either

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Error

import Misc
import qualified Lex as L
import Syntax


parse :: Monad m => String -> String -> ExceptT Error m (Module ())
parse name source = hoist generalize $ liftEither $ left parseErrorToError $ P.parse mainParser name source

parseErrorToError :: ParseError -> Error
parseErrorToError parseError =
  let loc = sourcePosToLoc $ errorPos parseError in
  let errorMessage = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages parseError) in
  (errorMessage, Just loc)

mainParser :: Parser (Module ())
mainParser = do
  L.parseWhiteSpace
  (modules, definitions) <- parseModuleContents
  eof
  return $ Module "Main" modules definitions

parseModuleContents :: Parser ([Module ()], [Def ()])
parseModuleContents = do
  (defs, submodules) <- fmap partitionEithers $ many $ parseEither parseDefinition parseModule
  return $ (submodules, defs)

parseModule :: Parser (Module ())
parseModule = do
  L.parseReserved "module"
  moduleName <- L.parseIdentifier
  L.parseReserved "begin"
  (submodules, defs) <- parseModuleContents
  L.parseReserved "end"
  return $ Module moduleName submodules defs


--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

parseType :: Parser Type
parseType = parseUnitType
  <|> parseIntType
  <|> parseFloatType

parseUnitType :: Parser Type
parseUnitType = do
  L.parseReserved "Unit"
  return TypeUnit

parseIntType :: Parser Type
parseIntType = do
  L.parseReserved "Int"
  return TypeInt

parseFloatType :: Parser Type
parseFloatType = do
  L.parseReserved "Float"
  return TypeFloat


--------------------------------------------------------------------------------
-- definitions
--------------------------------------------------------------------------------

parseDefinition :: Parser (Def ())
parseDefinition = try parseFunction

parseFunction :: Parser (Def ())
parseFunction = do
  loc <- sourcePosToLoc <$> getPosition
  L.parseReserved "def"
  name <- L.parseIdentifier
  let symbol = Symbol name []
  args <- L.parseParens $ L.parseCommaSep parseArg
  L.parseReserved ":"
  functionType <- parseType
  L.parseReserved "="
  body <- parseExpr
  return $ Function symbol functionType args body loc

parseArg :: Parser (Name, Type)
parseArg = do
  argName <- L.parseIdentifier
  L.parseReserved ":"
  argType <- parseType
  return (argName, argType)


--------------------------------------------------------------------------------
-- expressions
--------------------------------------------------------------------------------

parseExpr :: Parser (Expr ())
parseExpr = buildExpressionParser operatorTable parseFactor

parseFactor :: Parser (Expr ())
parseFactor = try parseUnit
  <|> try parseFloat
  <|> try parseInteger
  <|> try parseCall
  <|> parseVariable
  <|> parseIf
  <|> parseDoBlock
  <|> L.parseParens parseExpr

operatorTable :: OperatorTable String () Identity (Expr ())
operatorTable = [
    [binaryOperator "*." AssocLeft, binaryOperator "/." AssocLeft],
    [binaryOperator "+" AssocLeft, binaryOperator "-" AssocLeft, binaryOperator "+." AssocLeft, binaryOperator "-." AssocLeft],
    [binaryOperator "<" AssocLeft, binaryOperator "<." AssocLeft]
  ]

binaryOperator :: String -> Assoc -> Operator String () Identity (Expr ())
binaryOperator name assoc = Infix (parseBinaryOperator name) assoc

parseBinaryOperator :: String -> Parser (Expr () -> Expr () -> Expr ())
parseBinaryOperator name = do
  loc <- sourcePosToLoc <$> getPosition
  let symbol = Symbol name []
  L.parseReservedOperator name
  return $ \x y -> Call symbol [x, y] () loc

parseUnit :: Parser (Expr ())
parseUnit = do
  loc <- sourcePosToLoc <$> getPosition
  L.parseReserved "()"
  return $ Unit () loc

parseInteger :: Parser (Expr ())
parseInteger = do
  loc <- sourcePosToLoc <$> getPosition
  value <- L.parseInteger
  return $ Int value () loc

parseFloat :: Parser (Expr ())
parseFloat = do
  loc <- sourcePosToLoc <$> getPosition
  value <- L.parseFloat
  return $ Float value () loc

parseVariable :: Parser (Expr ())
parseVariable = do
  loc <- sourcePosToLoc <$> getPosition
  name <- L.parseIdentifier
  return $ Var name () loc

parseIf :: Parser (Expr ())
parseIf = do
  loc <- sourcePosToLoc <$> getPosition
  L.parseReserved "if"
  condition <- parseExpr
  L.parseReserved "then"
  ifTrue <- parseExpr
  L.parseReserved "else"
  ifFalse <- parseExpr
  return $ If condition ifTrue ifFalse () loc

parseCall :: Parser (Expr ())
parseCall = do
  loc <- sourcePosToLoc <$> getPosition
  symbolPath <- parseSymbolPath
  name <- L.parseIdentifier
  args <- L.parseParens $ L.parseCommaSep parseExpr
  let symbol = Symbol name symbolPath
  return $ Call symbol args () loc

parseSymbolPath :: Parser SymbolPath
parseSymbolPath = many $ try $ do
  symbolPath <- L.parseIdentifier
  L.parseDot
  return symbolPath

parseDoBlock :: Parser (Expr ())
parseDoBlock = do
  loc <- sourcePosToLoc <$> getPosition
  L.parseReserved "do"
  statements <- many parseStatement
  L.parseReserved "end"
  return $ Do statements () loc


--------------------------------------------------------------------------------
-- statements
--------------------------------------------------------------------------------

parseStatement :: Parser (Statement ())
parseStatement = try parseExpressionStatement
  <|> parseLetStatement

parseExpressionStatement :: Parser (Statement ())
parseExpressionStatement = do
  loc <- sourcePosToLoc <$> getPosition
  expression <- parseExpr
  return $ Expr expression () loc

parseLetStatement :: Parser (Statement ())
parseLetStatement = do
  loc <- sourcePosToLoc <$> getPosition
  L.parseReserved "let"
  name <- L.parseIdentifier
  L.parseReserved ":"
  letType <- parseType
  L.parseReserved "="
  expression <- parseExpr
  return $ Let name letType expression () loc


--------------------------------------------------------------------------------
-- misc
--------------------------------------------------------------------------------

sourcePosToLoc :: SourcePos -> Loc
sourcePosToLoc sourcePos = FileLineLocation (sourceName sourcePos) (sourceLine sourcePos)

