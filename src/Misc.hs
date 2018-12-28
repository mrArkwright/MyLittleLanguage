module Misc where

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

buildFolder :: String
buildFolder = "build"

inBuildFolder :: String -> String
inBuildFolder path = buildFolder ++ "/" ++ path

locDescription :: Loc -> String
locDescription (FileLineLocation sourceName lineNumber) = sourceName ++ ", line " ++ show lineNumber ++ ": "

