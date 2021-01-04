module Utils where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except



data Target
  = NativeTarget
  | EmbeddedTarget String String
  | ArduinoTarget String String

data Loc = FileLineLocation String Int
  deriving (Eq, Ord, Show)

type Error = (String, Maybe Loc)


lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x : []) = Just x
lastMaybe (_ : xs) = lastMaybe xs


maybeToError :: MonadError e m => Maybe a -> e -> m a
maybeToError (Just x) _ = return x
maybeToError Nothing err = throwError err


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
