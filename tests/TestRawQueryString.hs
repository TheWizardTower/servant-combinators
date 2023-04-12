module TestRawQueryString where

import Data.ByteString
import Network.HTTP.Client (method)
import Servant
import Servant.API.RawQueryString
import Test.QuickCheck.Monadic (PropertyM (..), assert, monadicIO)
import Test.Tasty
import TestLib (returns400, success)

import qualified Network.HTTP.Simple as S
import qualified Test.Tasty.QuickCheck as QC

type RawQueryStrAPI =
  "check-raw-query" :> RawQueryString :> Get '[JSON] NoContent

rawQueryStrServer :: Server RawQueryStrAPI
rawQueryStrServer = rawQuery
  where
    rawQuery :: ByteString -> Handler NoContent
    rawQuery rQuery = do
      case rQuery == "?test_value=foobar" of
        True -> pure NoContent
        False -> throwError err400 {errBody = "Invalid query flag passed."}

rawQueryStrProps :: Int -> TestTree
rawQueryStrProps port =
  testGroup
    "RawQueryString"
    [ QC.testProperty "Raw query endpoint recognizes the correct string" $
        monadicIO $ do
          result <- (fetchEndpoint port "?test_value=foobar") >>= success
          assert $ result == True
    , QC.testProperty "Raw query endpoint rejects incorrect string" $
        monadicIO $ do
          result <- (fetchEndpoint port "?invalid") >>= returns400
          assert $ result == True
    ]
  where
    fetchEndpoint :: Int -> String -> PropertyM IO (S.Response ByteString)
    fetchEndpoint port' value = do
      let initReq =
            S.parseRequest_ $
              "http://localhost:"
                <> (show port')
                <> "/check-raw-query"
                <> value
          req = initReq {method = "GET"}
       in do
            resp <- S.httpBS req
            pure resp
