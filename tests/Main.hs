module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import ServantExtras.Cookies (SessionMap)
import Test.Tasty
import TestCookies (CookieAPI, cookieProps, cookieServer)
import TestHeaders (HeaderAPI, headerProps, headerServer)
import TestLib (testFunctionGeneric)
import TestPath (PathAPI, pathProps, pathServer)

import qualified Data.Vault.Lazy as Vault

type TopLevelAPI =
  CookieAPI
    :<|> HeaderAPI
    :<|> PathAPI

topLevelServer :: Server TopLevelAPI
topLevelServer =
  cookieServer
    :<|> headerServer
    :<|> pathServer

topLevelProps :: Int -> TestTree
topLevelProps port =
  testGroup
    "Main tests"
    [ cookieProps port
    , headerProps port
    , pathProps port
    ]

mkTestApplication :: IO Application
mkTestApplication = do
  key <- Vault.newKey :: IO (Vault.Key SessionMap)
  pure $
    serveWithContext
      (Proxy @TopLevelAPI)
      (key :. EmptyContext)
      topLevelServer

runTopLevelServer :: Int -> IO ()
runTopLevelServer port = do
  app <- mkTestApplication
  run port app

main :: IO ()
main =
  testFunctionGeneric runTopLevelServer topLevelProps 8080
