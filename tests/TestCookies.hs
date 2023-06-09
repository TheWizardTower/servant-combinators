{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCookies where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (method)
import Network.Wai
import Servant
import Servant.API.Cookies
import Servant.Server.Internal.Delayed (addAcceptCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal, withRequest)
import Test.QuickCheck.Monadic (PropertyM (..), assert, monadicIO)
import Test.Tasty
import TestLib (returns400, success)
import Web.ClientSession
import Web.Cookie

import qualified Data.Map.Strict as Map
import qualified Data.Vault.Lazy as Vault
import qualified Network.HTTP.Simple as S
import qualified Network.HTTP.Types.Header as NTH
import qualified Test.Tasty.QuickCheck as QC

data CheckTestCookie

instance
  ( HasServer api ctx
  , HasContextEntry ctx HasCookies
  , HasContextEntry ctx (Vault.Key SessionMap)
  ) =>
  HasServer (CheckTestCookie :> api) ctx
  where
  type ServerT (CheckTestCookie :> api) m = ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt server

  route _ ctx server =
    route (Proxy @api) ctx $
      server `addAcceptCheck` checkTestCookie
    where
      checkTestCookie :: DelayedIO ()
      checkTestCookie = withRequest $ \req -> do
        let key = getContextEntry ctx :: Vault.Key SessionMap
            vlookup = Vault.lookup key (vault req) >>= Map.lookup "TEST_COOKIE"
        case vlookup of
          Just _ -> pure ()
          Nothing ->
            delayedFailFatal $
              err400
                { errBody = "TEST_COOKIE cookie not set."
                }

type CookieAPI =
  ProvideCookies '[Required]
    :> ( "add-cookie"
          :> Get '[JSON] (SetCookieHeader NoContent)
          :<|> "check-cookies"
            :> CheckTestCookie
            :> WithCookies '[Required]
            :> Get '[JSON] (Map Text Text)
       )

cookieServer :: Key -> Server CookieAPI
cookieServer key = addCookie key :<|> showCookie
  where
    addCookie :: Key -> Handler (SetCookieHeader NoContent)
    addCookie keyArg =
      let sMap =
            Map.fromList [("TEST_COOKIE", "FOOBAR")]
      in  liftIO $
            updateCookies
              keyArg
              sMap
              defaultSetCookie
              "servant_cookie"
              NoContent

    showCookie :: SessionMap -> Handler (Map Text Text)
    showCookie sMap = do
      let kvs = Map.toList sMap
      pure $ Map.fromList $ fmap (bimap decodeUtf8 decodeUtf8) kvs

cookieProps :: Key -> Int -> TestTree
cookieProps encKey port =
  testGroup
    "Cookies"
    [ QC.testProperty "Fetching a non-existent cookie returns a 400" $
        monadicIO $ do
          result <- (fetchCheckCookieEndpoint port Nothing) >>= returns400
          assert $ result == True
    , QC.testProperty "Calling add-cookie returns a 200, and adds a Set-Cookie header" $
        monadicIO $ do
          res1 <- (fetchCreateCookieEndpoint port)
          assert1 <- success res1
          assert $ assert1 == True
          let setCookieStr = lookup NTH.hSetCookie $ S.getResponseHeaders res1
              setCookieParsed = setCookieValue . parseSetCookie <$> setCookieStr
              sCookieVal = setCookieParsed >>= decrypt encKey
          assert $ sCookieVal == Just "TEST_COOKIE=FOOBAR"
    , QC.testProperty "Fetching the create-cookie, then check-cookie endpoints should work" $
        monadicIO $ do
          res1 <- fetchCreateCookieEndpoint port
          assert1 <- success res1
          assert $ assert1 == True
          encCookieVal <- liftIO $ encryptIO encKey "TEST_COOKIE=FOOBAR"
          let
            cookieHeader :: [NTH.Header]
            cookieHeader = [((mk "Cookie" :: NTH.HeaderName), encCookieVal)]
          res2 <- fetchCheckCookieEndpoint port $ Just cookieHeader
          assert2 <- success res2
          assert $ assert2 == True
    ]
  where
    fetchCheckCookieEndpoint :: Int -> Maybe [NTH.Header] -> PropertyM IO (S.Response ByteString)
    fetchCheckCookieEndpoint port' mHeader = do
      let initReq = S.parseRequest_ $ "http://localhost:" <> (show port') <> "/check-cookies"
          headerList = maybe [] id mHeader
          foldFunction pair reqAcc = uncurry S.addRequestHeader pair reqAcc
          reqHeader = foldr foldFunction initReq headerList
          req = reqHeader {method = "GET"}
       in do
            resp <- S.httpBS req
            pure resp

    fetchCreateCookieEndpoint :: Int -> PropertyM IO (S.Response ByteString)
    fetchCreateCookieEndpoint port' = do
      let initReq = S.parseRequest_ $ "http://localhost:" <> (show port') <> "/add-cookie"
          req = initReq {method = "GET"}
       in do
            resp <- S.httpBS req
            pure resp
