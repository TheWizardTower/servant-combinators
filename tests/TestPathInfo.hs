module TestPathInfo where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Network.HTTP.Client (method)
import Servant
import Servant.API.PathInfo
import Test.QuickCheck.Monadic (PropertyM (..), assert, monadicIO)
import Test.Tasty
import TestLib (returns400, success)

import qualified Network.HTTP.Simple as S
import qualified Test.Tasty.QuickCheck as QC

type PathInfoAPI =
  "merlin" :> "check-path-info" :> PathInfo :> Get '[JSON] NoContent
  :<|> "check-path-info" :> PathInfo :> Get '[JSON] NoContent

pathInfoServer :: Server PathInfoAPI
pathInfoServer = pathInfo :<|> pathInfo
  where
    pathInfo :: [Text] -> Handler NoContent
    pathInfo pInfo = do
      case elem "merlin" pInfo of
        False -> throwError err400 {errBody = "Merlin was not found in the path info list."}
        True -> pure NoContent

pathInfoProps :: Int -> TestTree
pathInfoProps port =
  testGroup
    "PathInfo"
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
