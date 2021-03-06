module Utils where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.List (stripPrefix)



data Target
  = NativeTarget
  | EmbeddedTarget String String
  | ArduinoTarget String String


data Loc = Loc {
    loc_sourceName :: String,
    loc_startLine :: Int,
    loc_startColumn :: Int,
    loc_endLine :: Int,
    loc_endColumn :: Int
  } deriving (Eq, Ord, Show)


type Error = (String, Phase, Maybe Loc)


data Phase = PhaseParse | PhaseRename | PhaseTypecheck | PhaseCodegen | PhaseCompile

instance Show Phase where
  show PhaseParse = "Parse"
  show PhaseRename = "Rename"
  show PhaseTypecheck = "Typecheck"
  show PhaseCodegen = "Codegen"
  show PhaseCompile = "Compile"


lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_ : xs) = lastMaybe xs


stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix s = fmap reverse $ stripPrefix (reverse suffix) (reverse s)


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
