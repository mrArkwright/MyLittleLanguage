module Parse.Parse (parse) where

import Control.Arrow (left)
import Control.Monad.Except

import Data.Functor.Identity
import Data.Either

import Text.Parsec hiding (parse, sourceName)
import qualified Text.Parsec as P (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr

import Utils
import Parse.Syntax
import Parse.Utils
import qualified Parse.Lex as Lex



parse :: MonadError Error m => String -> String -> String -> m Module
parse sourceName moduleName source = liftEither $ left parseErrorToError $ P.parse (mainParser moduleName) sourceName source


mainParser :: String -> Parser Module
mainParser moduleName = do

  Lex.parseWhiteSpace

  (modules, definitions) <- parseModuleContents

  eof

  return $ Module moduleName modules definitions


parseModuleContents :: Parser ([Module], [Definition])
parseModuleContents = do
  (defs, submodules) <- fmap partitionEithers $ many $ parseEither parseDefinition parseModule
  return (submodules, defs)


parseModule :: Parser Module
parseModule = do

  Lex.parseReserved "module"

  moduleName <- Lex.parseIdentifier

  Lex.parseReserved "begin"

  (submodules, defs) <- parseModuleContents

  Lex.parseReserved "end"

  return $ Module moduleName submodules defs



--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

parseType :: Parser Type
parseType = parseUnitType
  <|> parseIntType
  <|> parseInt8Type
  <|> parseFloatType
  <|> parseFunctionType


parseUnitType :: Parser Type
parseUnitType = do
  Lex.parseReserved "Unit"
  return TypeUnit


parseIntType :: Parser Type
parseIntType = do
  Lex.parseReserved "Int"
  return TypeInt


parseInt8Type :: Parser Type
parseInt8Type = do
  Lex.parseReserved "Int8"
  return TypeInt8


parseFloatType :: Parser Type
parseFloatType = do
  Lex.parseReserved "Float"
  return TypeFloat


parseFunctionType :: Parser Type
parseFunctionType = do
  parameterTypes <- Lex.parseParens $ Lex.parseCommaSep parseType
  Lex.parseReserved "->"
  returnType <- parseType
  return $ TypeFunction parameterTypes returnType



--------------------------------------------------------------------------------
-- definitions
--------------------------------------------------------------------------------

parseDefinition :: Parser Definition
parseDefinition = try $ do

  startPos <- getPosition

  Lex.parseReserved "let"

  name <- Lex.parseIdentifier

  parameters <- optionMaybe $ Lex.parseParens $ Lex.parseCommaSep parseParameter

  Lex.parseReserved ":"

  resultType <- parseType

  Lex.parseReserved "="

  expression <- parseExpression

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ Definition name parameters resultType expression loc


parseParameter :: Parser Parameter
parseParameter = do

  parameterName <- Lex.parseIdentifier

  Lex.parseReserved ":"

  parameterType <- parseType

  return $ Parameter parameterName parameterType



--------------------------------------------------------------------------------
-- expressions
--------------------------------------------------------------------------------

parseExpression :: Parser Expression
parseExpression = buildExpressionParser operatorTable parseFactor


parseFactor :: Parser Expression
parseFactor = try parseUnit
  <|> try parsePointer
  <|> try parseFloat
  <|> try parseInteger8
  <|> try parseInteger
  <|> try parseCall
  <|> parseSymbolReference
  <|> parseIf
  <|> parseDoBlock
  <|> Lex.parseParens parseExpression


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

  startPos <- getPosition

  Lex.parseReservedOperator name

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  let symbol = Symbol name []
  return $ \x y -> Call symbol [x, y] loc


parseUnit :: Parser Expression
parseUnit = do

  startPos <- getPosition

  Lex.parseReserved "()"

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ Unit loc


parsePointer :: Parser Expression
parsePointer = do

  startPos <- getPosition

  value <- Lex.parsePointer

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ LiteralExpression (LiteralPointer value) loc


parseInteger :: Parser Expression
parseInteger = do

  startPos <- getPosition

  value <- Lex.parseInteger

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ LiteralExpression (LiteralInt value) loc


parseInteger8 :: Parser Expression
parseInteger8 = do

  startPos <- getPosition

  value <- Lex.parseInteger8

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ LiteralExpression (LiteralInt8 value) loc


parseFloat :: Parser Expression
parseFloat = do

  startPos <- getPosition

  value <- Lex.parseFloat

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ LiteralExpression (LiteralFloat value) loc


parseSymbolReference :: Parser Expression
parseSymbolReference = do

  startPos <- getPosition

  name <- Lex.parseIdentifier

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  let symbol = Symbol name []
  return $ SymbolReference symbol loc


parseCall :: Parser Expression
parseCall = do

  startPos <- getPosition

  symbolPath <- parseSymbolPath

  name <- Lex.parseIdentifier

  args <- Lex.parseParens $ Lex.parseCommaSep parseExpression

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  let symbol = Symbol name symbolPath
  return $ Call symbol args loc


parseIf :: Parser Expression
parseIf = do

  startPos <- getPosition

  Lex.parseReserved "if"

  condition <- parseExpression

  Lex.parseReserved "then"

  ifTrue <- parseExpression

  Lex.parseReserved "else"

  ifFalse <- parseExpression

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ If condition ifTrue ifFalse loc


parseSymbolPath :: Parser SymbolPath
parseSymbolPath = many $ try $ do

  symbolPath <- Lex.parseIdentifier

  Lex.parseDot

  return symbolPath


parseDoBlock :: Parser Expression
parseDoBlock = do

  startPos <- getPosition

  Lex.parseReserved "do"

  statements <- many parseStatement

  Lex.parseReserved "end"

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ Do statements loc



--------------------------------------------------------------------------------
-- statements
--------------------------------------------------------------------------------

parseStatement :: Parser Statement
parseStatement = try parseExpressionStatement
  <|> parseDefinitionStatement


parseExpressionStatement :: Parser Statement
parseExpressionStatement = do

  startPos <- getPosition

  expression <- parseExpression

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ StatementExpression expression loc


parseDefinitionStatement :: Parser Statement
parseDefinitionStatement = do

  startPos <- getPosition

  definition <- parseDefinition

  endPos <- getPosition
  let loc = sourcePosToLoc startPos endPos

  return $ StatementDefinition definition loc
