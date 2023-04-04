module Main where

import Control.Monad (void)
import TestCookies (testFunction)
import TestHeaders (testFunction)

main :: IO ()
main = do
  void $ TestHeaders.testFunction 8080
  void $ TestCookies.testFunction 8080
