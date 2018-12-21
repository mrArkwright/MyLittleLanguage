module Main where

import Prelude

import Control.Monad
import Control.Exception

import System.Exit
import System.Directory
import System.Process
import System.IO.Error

import Test.Hspec

import Top


main :: IO ()
main = hspec $ beforeAll (setCurrentDirectory "test") $ do

  describe "Top" $ do

    specify "process example.mll" $ do
      let name = "example"
      let files = fmap ("build/" ++) [name ++ ".ll", name ++ ".s", name ++ ".o", "builtins.o", name]
      mapM_ removeFileIfExists files
      success <- processFile (name ++ ".mll")
      shouldBe success True
      (exitCode, stdOutput, _) <- readProcessWithExitCode ("./build/" ++ name) [] ""
      putStr stdOutput
      when (stdOutput == "" || last stdOutput /= '\n') $ putStr "%\n"
      shouldBe exitCode ExitSuccess

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
