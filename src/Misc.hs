module Misc where

import Data.Maybe
import Control.Monad.State

maybeError :: Maybe a -> String -> a
maybeError value errorString = fromMaybe (error errorString) value

runState' :: s -> State s a -> (a, s)
runState' = flip runState
