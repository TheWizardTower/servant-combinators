module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Cookies (SessionMap)
import Test.Tasty
import TestCookies (CookieAPI, cookieProps, cookieServer)
import TestHeaders (HeaderAPI, headerProps, headerServer)
import TestLib (testFunctionGeneric)
import TestQueryString (QueryStrAPI, queryStrProps, queryStrServer)
import TestRawQueryString (RawQueryStrAPI, rawQueryStrProps, rawQueryStrServer)
import Web.ClientSession

import qualified Data.Vault.Lazy as Vault

type TopLevelAPI =
  CookieAPI
    :<|> HeaderAPI
    :<|> QueryStrAPI
    :<|> RawQueryStrAPI

topLevelServer :: Key -> Server TopLevelAPI
topLevelServer encKey =
  cookieServer encKey
    :<|> headerServer
    :<|> queryStrServer
    :<|> rawQueryStrServer

topLevelProps :: Key -> Int -> TestTree
topLevelProps key port =
  testGroup
    "Main tests"
    [ cookieProps key port
    , headerProps port
    , queryStrProps port
    , rawQueryStrProps port
    ]

mkTestApplication :: Key -> IO Application
mkTestApplication encKey = do
  key <- Vault.newKey :: IO (Vault.Key SessionMap)
  pure $
    serveWithContext
      (Proxy @TopLevelAPI)
      (encKey :. key :. EmptyContext)
      (topLevelServer encKey)

runTopLevelServer :: Key -> Int -> IO ()
runTopLevelServer key port = do
  app <- mkTestApplication key
  run port app

main :: IO ()
main = do
  (_initKeyBS, encKey) <- randomKey
  testFunctionGeneric (runTopLevelServer encKey) (topLevelProps encKey) 8080
