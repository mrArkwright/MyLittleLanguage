module Misc where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Maybe

import Text.Parsec (try, optionMaybe)
import Text.Parsec.String (Parser)



data Loc = FileLineLocation String Int
  deriving (Eq, Ord, Show)

type Error = (String, Maybe Loc)


lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x : []) = Just x
lastMaybe (_ : xs) = lastMaybe xs


maybeError :: Maybe a -> String -> a
maybeError value errorString = fromMaybe (error errorString) value


maybeToExcept :: MonadError e m => Maybe a -> e -> m a
maybeToExcept (Just x) _ = return x
maybeToExcept Nothing err = throwError err


infixl 5 -:+
(-:+) :: [a] -> a -> [a]
(-:+) xs x = xs ++ [x]


zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex xs = zip xs [(0 :: Int)..]


runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT


evalStateT' :: Monad m => s -> StateT s m a -> m a
evalStateT' = flip evalStateT


execStateT' :: Monad m => s -> StateT s m a -> m s
execStateT' = flip execStateT


buildFolder :: String
buildFolder = "build"


inBuildFolder :: String -> String
inBuildFolder path = buildFolder ++ "/" ++ path


locDescription :: Loc -> String
locDescription (FileLineLocation sourceName lineNumber) = sourceName ++ ", line " ++ show lineNumber ++ ": "


parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither parseA parseB = do

  parsedA <- optionMaybe $ try parseA

  case parsedA of

    Just parsedA' -> return $ Left parsedA'

    Nothing -> do
      parsedB <- parseB
      return $ Right parsedB

