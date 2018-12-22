module Misc where

import Data.Maybe



lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x : []) = Just x
lastMaybe (_ : xs) = lastMaybe xs

maybeError :: Maybe a -> String -> a
maybeError value errorString = fromMaybe (error errorString) value

buildFolder :: String
buildFolder = "build"

inBuildFolder :: String -> String
inBuildFolder path = buildFolder ++ "/" ++ path

