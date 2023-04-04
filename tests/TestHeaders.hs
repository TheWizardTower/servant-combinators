module TestHeaders where

import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Network.HTTP.Client (method)
import Network.Wai.Handler.Warp (run)
import Servant
import ServantExtras.HeaderList
import Test.QuickCheck.Monadic (PropertyM (..), assert, monadicIO)
import Test.Tasty
import TestLib (returns400, success, testFunctionGeneric)

import qualified Network.HTTP.Simple as S
import qualified Network.HTTP.Types.Header as NTH
import qualified Test.Tasty.QuickCheck as QC

myHeaderList :: [NTH.Header]
myHeaderList =
  [
    ( (mk "MerlinWasHere" :: NTH.HeaderName)
    , "TEST_COOKIE=FOOBAR"
    )
  ,
    ( (mk "OtherHeader" :: NTH.HeaderName)
    , "FOOBAR"
    )
  ]
type TestAPI =
  "check-headers"
    :> HeaderList
    :> Get '[JSON] NoContent

testServer :: Server TestAPI
testServer = checkHeader
  where
    checkHeader :: [NTH.Header] -> Handler NoContent
    checkHeader headers = do
      let headerIntersection = all (\a -> elem a headers) myHeaderList
      case headerIntersection of
        True -> do
          pure NoContent
        False -> do
          throwError err400

mkTestApplication :: IO Application
mkTestApplication = do
  pure $
    serve
      (Proxy @TestAPI)
      -- (key :. EmptyContext)
      testServer

runHeaderServer :: Int -> IO ()
runHeaderServer port = do
  app <- mkTestApplication
  run port app

headerProps :: Int -> TestTree
headerProps port =
  testGroup
    "HeaderList"
    [ QC.testProperty "The endpoint should error with no headers present" $
        monadicIO $ do
          result <- (fetchHeaderEndpoint port Nothing) >>= returns400
          assert $ result == True
    , QC.testProperty "The endpoint should return a 200 if a header is added." $
        monadicIO $ do
          result <- (fetchHeaderEndpoint port (Just myHeaderList)) >>= success
          assert $ result == True
    ]
  where
    fetchHeaderEndpoint :: Int -> Maybe [NTH.Header] -> PropertyM IO (S.Response ByteString)
    fetchHeaderEndpoint port' mHeaderList =
      let initReq =
            S.parseRequest_ $
              "http://localhost:"
                <> (show port')
                <> "/check-headers"
          headerList = maybe [] id mHeaderList
          reqHeader = S.setRequestHeaders headerList initReq
          req = reqHeader {method = "GET"}
      in  do
            resp <- S.httpBS req
            pure resp

testFunction :: Int -> IO ()
testFunction port = testFunctionGeneric runHeaderServer headerProps port
