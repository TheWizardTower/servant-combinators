module TestPath where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.HTTP.Client (method)
import Servant
import ServantExtras.Path
import Test.QuickCheck.Monadic (PropertyM (..), assert, monadicIO)
import Test.Tasty
import TestLib (returns400, success)

import qualified Network.HTTP.Simple as S
import qualified Test.Tasty.QuickCheck as QC

type PathAPI =
  "test"
    :> "check-path"
    :> PathInfo
    :> Get '[JSON] NoContent

pathServer :: Server PathAPI
pathServer = checkPath
  where
    checkPath :: [Text] -> Handler NoContent
    checkPath pInfo = do
      liftIO $ do
        putStrLn "Printing pInfo"
        mapM_ print pInfo
        putStrLn "pInfo over."
      case False of
        True -> do
          pure NoContent
        False -> do
          throwError err400

pathProps :: Int -> TestTree
pathProps port =
  testGroup
    "PathInfo"
    [ QC.testProperty "The endpoint should Do Something(tm)" $
        monadicIO $ do
          result <- (fetchHeaderEndpoint port Nothing) >>= returns400
          assert $ result == True
    , QC.testProperty "The endpoint should return a 200 if a header is added." $
        monadicIO $ do
          result <- (fetchHeaderEndpoint port (Just [])) >>= success
          assert $ result == True
    ]
  where
    fetchHeaderEndpoint :: Int -> Maybe [Text] -> PropertyM IO (S.Response ByteString)
    fetchHeaderEndpoint port' mHeaderList =
      let initReq =
            S.parseRequest_ $
              "http://localhost:"
                <> (show port')
                <> "/test"
                <> "/check-path"
          _pathList = maybe [] id mHeaderList
          req = initReq {method = "GET"}
      in  do
            resp <- S.httpBS req
            pure resp
