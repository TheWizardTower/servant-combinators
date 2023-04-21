module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Cookies (SessionMap)
import Test.Tasty
import TestCookies (CookieAPI, cookieProps, cookieServer)
import TestHeaders (HeaderAPI, headerProps, headerServer)
import TestLib (testFunctionGeneric)
import TestPathInfo (PathInfoAPI, pathInfoProps, pathInfoServer)
import TestQueryString (QueryStrAPI, queryStrProps, queryStrServer)
import TestRawPathInfo (RawPathInfoAPI, rawPathInfoProps, rawPathInfoServer)
import TestRawQueryString (RawQueryStrAPI, rawQueryStrProps, rawQueryStrServer)
import TestRawRequest (RawRequestAPI, rawRequestProps, rawRequestServer)
import Web.ClientSession

import qualified Data.Vault.Lazy as Vault

type TopLevelAPI =
  CookieAPI
    :<|> HeaderAPI
    :<|> QueryStrAPI
    :<|> RawQueryStrAPI
    :<|> RawRequestAPI
    :<|> PathInfoAPI
    :<|> RawPathInfoAPI

topLevelServer :: Key -> Server TopLevelAPI
topLevelServer encKey =
  cookieServer encKey
    :<|> headerServer
    :<|> queryStrServer
    :<|> rawQueryStrServer
    :<|> rawRequestServer
    :<|> pathInfoServer
    :<|> rawPathInfoServer

topLevelProps :: Key -> Int -> TestTree
topLevelProps key port =
  testGroup
    "Main tests"
    [ cookieProps key port
    , headerProps port
    , queryStrProps port
    , rawQueryStrProps port
    , rawRequestProps port
    , pathInfoProps port
    , rawPathInfoProps port
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
