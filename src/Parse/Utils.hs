module Parse.Utils where

import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Error

import Utils


parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither parseA parseB = do

  parsedA <- optionMaybe $ try parseA

  case parsedA of

    Just parsedA' -> return $ Left parsedA'

    Nothing -> do
      parsedB <- parseB
      return $ Right parsedB


sourcePosToLoc :: SourcePos -> Loc
sourcePosToLoc sourcePos = FileLineLocation (sourceName sourcePos) (sourceLine sourcePos)


parseErrorToError :: ParseError -> Error
parseErrorToError parseError =

  let loc = sourcePosToLoc $ errorPos parseError in
  let errorMessage = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages parseError) in

  (errorMessage, Just loc)
