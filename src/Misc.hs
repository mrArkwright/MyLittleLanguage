module Misc where

import Data.Maybe
import Control.Monad.State

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x : []) = Just x
lastMaybe (x : xs) = lastMaybe xs

maybeError :: Maybe a -> String -> a
maybeError value errorString = fromMaybe (error errorString) value

