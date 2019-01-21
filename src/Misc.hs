module Misc where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Maybe



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

