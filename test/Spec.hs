module Main where

import Prelude

import Control.Exception

import System.Directory
import System.IO.Error

import Test.Hspec

import Top


main :: IO ()
main = hspec $ do

  describe "Top" $ do
    specifyCompileExample "native_example" NativeTarget
    specifyCompileExample "LPC810_blinky" (EmbeddedTarget "thumbv6m-none--eabi" "cortex-m0")
    specifyCompileExample "arduino_blinky" (ArduinoTarget "avr-atmel-none" "atmega328p")


specifyCompileExample :: String -> Target -> Spec
specifyCompileExample name target = specify ("compile " ++ name) $ withCurrentDirectory ("examples/" ++ name) $ do

  let files = fmap ("build/" ++) [name ++ ".ll", name ++ ".s", name ++ ".o"]
  mapM_ removeFileIfExists files

  let options = Top.defaultOptions {
      _target = target
    }

  success <- processFile options (name ++ ".mll")

  shouldBe success True


removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
