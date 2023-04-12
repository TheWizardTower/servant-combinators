module TestRawRequest where

import Data.ByteString
import Data.CaseInsensitive (mk)
import Network.HTTP.Client (method)
import Network.Wai (Request (..))
import Servant
import Servant.API.RawRequest
import Test.QuickCheck.Monadic (PropertyM (..), assert, monadicIO)
import Test.Tasty
import TestLib (returns400, success)

import qualified Network.HTTP.Simple as S
import qualified Network.HTTP.Types.Header as NTH
import qualified Test.Tasty.QuickCheck as QC

type RawRequestAPI =
  "check-raw-request" :> RawRequest :> Get '[JSON] NoContent

rawRequestServer :: Server RawRequestAPI
rawRequestServer = rawRequest
  where
    rawRequest :: Request -> Handler NoContent
    rawRequest req = do
      let reqHeaders = requestHeaders req
          reqQS = queryString req
      case (lookup "testHeader" reqHeaders, reqQS == [("test_query", Just "val")]) of
        (Just "foo", True) -> pure NoContent
        (_, _) -> throwError err400 {errBody = "Missing value"}

headerList :: [NTH.Header]
headerList = [(mk "testHeader", "foo")]

rawRequestProps :: Int -> TestTree
rawRequestProps port =
  testGroup
    "RawRequest"
    [ QC.testProperty "Raw Request holds the headers, query string of the original request." $
        monadicIO $ do
          result <- (fetchEndpoint port "?test_query=val" headerList) >>= success
          assert $ result == True
    , QC.testProperty "Raw Reuest rejects a request with missing headers" $
        monadicIO $ do
          result <- (fetchEndpoint port "?test_query=val" []) >>= returns400
          assert $ result == True
    , QC.testProperty "Raw Reuest rejects a request with missing query string" $
        monadicIO $ do
          result <- (fetchEndpoint port "?invalid" headerList) >>= returns400
          assert $ result == True
    ]
  where
    fetchEndpoint :: Int -> String -> [NTH.Header] -> PropertyM IO (S.Response ByteString)
    fetchEndpoint port' queryVal hList = do
      let initReq =
            S.parseRequest_ $
              "http://localhost:"
                <> (show port')
                <> "/check-raw-request"
                <> queryVal
          reqHeader = S.setRequestHeaders hList initReq
          req = reqHeader {method = "GET"}
       in do
            resp <- S.httpBS req
            pure resp
