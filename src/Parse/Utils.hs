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


sourcePosToLoc :: SourcePos -> SourcePos -> Loc
sourcePosToLoc startPos endPos = Loc {
    loc_sourceName = sourceName startPos,
    loc_startLine = sourceLine startPos,
    loc_startColumn = sourceColumn startPos,
    loc_endLine = sourceLine endPos,
    loc_endColumn = sourceColumn endPos
  }


parseErrorToError :: ParseError -> Error
parseErrorToError parseError =

  let loc = sourcePosToLoc (errorPos parseError) (errorPos parseError) in
  let errorMessage = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages parseError) in

  (errorMessage, phase, Just loc)


phase :: Phase
phase = PhaseParse
