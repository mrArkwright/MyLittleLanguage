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

    specify "compile and run example.mll" $ do

      let name = "example"

      let files = fmap ("build/" ++) [name ++ ".ll", name ++ ".s", name ++ ".o", "builtins.o", name]
      mapM_ removeFileIfExists files

      let options = Top.defaultOptions

      success <- processFile options (name ++ ".mll")

      shouldBe success True

      (exitCode, stdOutput, _) <- readProcessWithExitCode ("./build/" ++ name) [] ""
      putStr stdOutput
      when (stdOutput == "" || last stdOutput /= '\n') $ putStr "%\n"

      shouldBe exitCode ExitSuccess

    specify "compile LPC810_blinky.mll" $ do

      let name = "LPC810_blinky"

      let files = fmap ("build/" ++) [name ++ ".ll", name ++ ".s", name ++ ".o"]
      mapM_ removeFileIfExists files

      let options = Top.defaultOptions {
          _target = EmbeddedTarget "thumbv6m-none--eabi" "cortex-m0"
        }

      success <- processFile options (name ++ ".mll")

      shouldBe success True

    specify "compile arduino_blinky.mll" $ do

      let name = "arduino_blinky"

      let files = fmap ("build/" ++) [name ++ ".ll", name ++ ".s", name ++ ".o"]
      mapM_ removeFileIfExists files

      let options = Top.defaultOptions {
          _target = ArduinoTarget "avr-atmel-none" "atmega328p"
        }

      success <- processFile options (name ++ ".mll")

      shouldBe success True

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
