module TestRawPathInfo where

import Data.ByteString (ByteString)
import Network.HTTP.Client (method)
import Servant
import Servant.API.RawPathInfo
import Test.QuickCheck.Monadic (PropertyM (..), assert, monadicIO)
import Test.Tasty
import TestLib (returns400, success)

import qualified Network.HTTP.Simple as S
import qualified Test.Tasty.QuickCheck as QC

type RawPathInfoAPI =
  "merlin" :> "check-path-info" :> RawPathInfo :> Get '[JSON] NoContent
  :<|> "check-path-info" :> RawPathInfo :> Get '[JSON] NoContent

rawPathInfoServer :: Server RawPathInfoAPI
rawPathInfoServer = pathInfo :<|> pathInfo
  where
    pathInfo :: ByteString -> Handler NoContent
    pathInfo rPathInfo = do
      case rPathInfo of
        "merlin/check-path-info" -> pure NoContent
        _ -> throwError err400 {errBody = "Merlin was not found in the path info list."}

rawPathInfoProps :: Int -> TestTree
rawPathInfoProps port =
  testGroup
    "RawPathInfo"
    [ QC.testProperty "Requests to the merlin/check-path-info should succeed with a 200" $
        monadicIO $ do
          result <- (fetchEndpoint port "/merlin") >>= success
          assert $ result == True
    , QC.testProperty "Requests to just /check-path-info should fail with a 400" $
        monadicIO $ do
          result <- (fetchEndpoint port "") >>= returns400
          assert $ result == True
    ]
  where
    fetchEndpoint :: Int -> String -> PropertyM IO (S.Response ByteString)
    fetchEndpoint port' urlPrefix = do
      let initReq =
            S.parseRequest_ $
              "http://localhost:"
                <> (show port')
                <> urlPrefix
                <> "/check-path-info"
          req = initReq {method = "GET"}
       in do
            resp <- S.httpBS req
            pure resp
