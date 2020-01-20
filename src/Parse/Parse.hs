module Parse.Parse (parse) where

import Control.Arrow (left)
import Control.Monad.Except

import Data.Functor.Identity
import Data.Either

import Text.Parsec hiding (parse)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Error

import Misc
import qualified Parse.Lex as L
import Parse.Syntax



parse :: MonadError Error m => String -> String -> m Module
parse name source = liftEither $ left parseErrorToError $ P.parse mainParser name source


parseErrorToError :: ParseError -> Error
parseErrorToError parseError =

  let loc = sourcePosToLoc $ errorPos parseError in
  let errorMessage = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages parseError) in

  (errorMessage, Just loc)


mainParser :: Parser Module
mainParser = do

  L.parseWhiteSpace

  (modules, definitions) <- parseModuleContents

  eof

  return $ Module "Main" modules definitions


parseModuleContents :: Parser ([Module], [Definition])
parseModuleContents = do
  (defs, submodules) <- fmap partitionEithers $ many $ parseEither parseDefinition parseModule
  return $ (submodules, defs)


parseModule :: Parser Module
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

parseDefinition :: Parser Definition
parseDefinition = try $ do

  loc <- sourcePosToLoc <$> getPosition

  L.parseReserved "let"

  name <- L.parseIdentifier

  parameters <- optionMaybe $ L.parseParens $ L.parseCommaSep parseParameter

  L.parseReserved ":"

  resultType <- parseType

  L.parseReserved "="

  expression <- parseExpression

  return $ Definition name parameters resultType expression loc


parseParameter :: Parser Parameter
parseParameter = do

  parameterName <- L.parseIdentifier

  L.parseReserved ":"

  parameterType <- parseType

  return $ Parameter parameterName parameterType



--------------------------------------------------------------------------------
-- expressions
--------------------------------------------------------------------------------

parseExpression :: Parser Expression
parseExpression = buildExpressionParser operatorTable parseFactor


parseFactor :: Parser Expression
parseFactor = try parseUnit
  <|> try parseFloat
  <|> try parseInteger
  <|> try parseCall
  <|> parseSymbolReference
  <|> parseIf
  <|> parseDoBlock
  <|> L.parseParens parseExpression


operatorTable :: OperatorTable String () Identity Expression
operatorTable = [
    [binaryOperator "*." AssocLeft, binaryOperator "/." AssocLeft],
    [binaryOperator "+" AssocLeft, binaryOperator "-" AssocLeft, binaryOperator "+." AssocLeft, binaryOperator "-." AssocLeft],
    [binaryOperator "<" AssocLeft, binaryOperator "<." AssocLeft]
  ]


binaryOperator :: String -> Assoc -> Operator String () Identity Expression
binaryOperator name assoc = Infix (parseBinaryOperator name) assoc


parseBinaryOperator :: String -> Parser (Expression -> Expression -> Expression)
parseBinaryOperator name = do

  loc <- sourcePosToLoc <$> getPosition

  L.parseReservedOperator name

  let symbol = Symbol name []
  return $ \x y -> Call symbol [x, y] loc


parseUnit :: Parser Expression
parseUnit = do

  loc <- sourcePosToLoc <$> getPosition

  L.parseReserved "()"
  return $ Unit loc


parseInteger :: Parser Expression
parseInteger = do

  loc <- sourcePosToLoc <$> getPosition

  value <- L.parseInteger
  return $ Int value loc


parseFloat :: Parser Expression
parseFloat = do

  loc <- sourcePosToLoc <$> getPosition

  value <- L.parseFloat
  return $ Float value loc


parseSymbolReference :: Parser Expression
parseSymbolReference = do

  loc <- sourcePosToLoc <$> getPosition

  name <- L.parseIdentifier
  let symbol = Symbol name []
  return $ SymbolReference symbol loc


parseCall :: Parser Expression
parseCall = do

  loc <- sourcePosToLoc <$> getPosition

  symbolPath <- parseSymbolPath

  name <- L.parseIdentifier

  args <- L.parseParens $ L.parseCommaSep parseExpression

  let symbol = Symbol name symbolPath
  return $ Call symbol args loc


parseIf :: Parser Expression
parseIf = do

  loc <- sourcePosToLoc <$> getPosition

  L.parseReserved "if"

  condition <- parseExpression

  L.parseReserved "then"

  ifTrue <- parseExpression

  L.parseReserved "else"

  ifFalse <- parseExpression

  return $ If condition ifTrue ifFalse loc


parseSymbolPath :: Parser SymbolPath
parseSymbolPath = many $ try $ do

  symbolPath <- L.parseIdentifier

  L.parseDot

  return symbolPath


parseDoBlock :: Parser Expression
parseDoBlock = do

  loc <- sourcePosToLoc <$> getPosition

  L.parseReserved "do"

  statements <- many parseStatement

  L.parseReserved "end"

  return $ Do statements loc



--------------------------------------------------------------------------------
-- statements
--------------------------------------------------------------------------------

parseStatement :: Parser Statement
parseStatement = try parseExpressionStatement
  <|> parseDefinitionStatement


parseExpressionStatement :: Parser Statement
parseExpressionStatement = do

  loc <- sourcePosToLoc <$> getPosition

  expression <- parseExpression

  return $ StatementExpression expression loc


parseDefinitionStatement :: Parser Statement
parseDefinitionStatement = do

  loc <- sourcePosToLoc <$> getPosition

  definition <- parseDefinition

  return $ StatementDefinition definition loc



--------------------------------------------------------------------------------
-- misc
--------------------------------------------------------------------------------

sourcePosToLoc :: SourcePos -> Loc
sourcePosToLoc sourcePos = FileLineLocation (sourceName sourcePos) (sourceLine sourcePos)
