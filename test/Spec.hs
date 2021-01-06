module Main where

import Prelude

import Control.Exception
import Control.Monad.Except

import Data.Either

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
specifyCompileExample name target = specify ("compile example " ++ name) $ withCurrentDirectory ("examples/" ++ name) $ do

  let options = Top.defaultOptions {
      _target = target
    }

  result <- runExceptT $ compileFile options "Main.mll"

  case result of
    Left err -> putStrLn err
    Right _ -> return ()

  let success = isRight result
  shouldBe success True


removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
