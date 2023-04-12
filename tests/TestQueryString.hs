module TestQueryString where

import Data.ByteString
import Network.HTTP.Client (method)
import Network.HTTP.Types
import Servant
import Servant.API.QueryString
import Test.QuickCheck.Monadic (PropertyM (..), assert, monadicIO)
import Test.Tasty
import TestLib (returns400, success)

import qualified Network.HTTP.Simple as S
import qualified Test.Tasty.QuickCheck as QC

type QueryStrAPI =
  "check-query-flag" :> QueryString :> Get '[JSON] NoContent
    :<|> "check-query-value" :> QueryString :> Get '[JSON] NoContent

queryStrServer :: Server QueryStrAPI
queryStrServer = queryFlag :<|> queryVal
  where
    queryFlag :: Query -> Handler NoContent
    queryFlag query = do
      case lookup "test_value" query of
        Nothing -> throwError err400 {errBody = "Omitted query flag"}
        Just flag -> case flag of
          Nothing -> pure NoContent -- value was a flag, as we expect
          Just _b -> throwError err400 {errBody = "Flag was given a value, incorrect"}

    queryVal :: Query -> Handler NoContent
    queryVal query = do
      case lookup "test_value" query of
        Nothing -> throwError err400 {errBody = "Omitted query key-valke pair"}
        Just val -> case val of
          Nothing -> throwError err400 {errBody = "Value in key-value pair query omitted, incorrect"}
          Just _b -> pure NoContent

queryStrProps :: Int -> TestTree
queryStrProps port =
  testGroup
    "QueryString"
    [ QC.testProperty "Query flags should have a 'Nothing' as a value." $
        monadicIO $ do
          result <- (fetchEndpoint port "flag" "") >>= success
          assert $ result == True
    , QC.testProperty "Query Flag endpoint should error if given a value" $
        monadicIO $ do
          result <- (fetchEndpoint port "flag" "=value") >>= returns400
          assert $ result == True
    , QC.testProperty "Value endpoint expects a value" $
        monadicIO $ do
          result <- (fetchEndpoint port "value" "=value") >>= success
          assert $ result == True
    , QC.testProperty "Value endponit errors if given a flag" $
        monadicIO $ do
          result <- (fetchEndpoint port "value" "") >>= returns400
          assert $ result == True
    ]
  where
    fetchEndpoint :: Int -> String -> String -> PropertyM IO (S.Response ByteString)
    fetchEndpoint port' flagOrVal value = do
      let initReq =
            S.parseRequest_ $
              "http://localhost:"
                <> (show port')
                <> "/check-query-"
                <> flagOrVal
                <> "?test_value"
                <> value
          req = initReq {method = "GET"}
       in do
            resp <- S.httpBS req
            pure resp
