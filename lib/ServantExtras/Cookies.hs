{-# LANGUAGE FlexibleContexts #-}

module ServantExtras.Cookies where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Header hiding (Header)
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (addHeaderCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFailFatal, withRequest)
import Web.ClientSession
import Web.Cookie

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Vault.Lazy as Vault

type SessionMap = Map ByteString ByteString

data ProvideCookies (mods :: [Type])

data WithCookies (mods :: [Type])

data HasCookies = HasCookies

data HasCookiesMaybe = HasCookiesMaybe

instance
  ( HasServer api (HasCookies ': ctx)
  , HasContextEntry ctx (Vault.Key SessionMap)
  ) =>
  HasServer (ProvideCookies '[Required] :> api) ctx
  where
  type ServerT (ProvideCookies '[Required] :> api) m = ServerT api m

  hoistServerWithContext _ _ nt server =
    hoistServerWithContext (Proxy @api) (Proxy @(HasCookies ': ctx)) nt server

  route _ ctx server =
    route (Proxy @api) (HasCookies :. ctx) server <&> \app req respK -> do
      let
        mCookie = lookup hCookie (requestHeaders req)
        cookies = maybe Map.empty (Map.fromList . parseCookies) mCookie
        key = getContextEntry ctx :: Vault.Key SessionMap
        req' = req {vault = Vault.insert key cookies (vault req)}
      app req' respK

instance
  ( HasServer api (HasCookiesMaybe ': ctx)
  , HasContextEntry ctx (Vault.Key (Maybe SessionMap))
  ) =>
  HasServer (ProvideCookies '[Optional] :> api) ctx
  where
  type ServerT (ProvideCookies '[Optional] :> api) m = ServerT api m

  hoistServerWithContext _ _ nt server =
    hoistServerWithContext (Proxy @api) (Proxy @(HasCookiesMaybe ': ctx)) nt server

  route _ ctx server =
    route (Proxy @api) ((HasCookiesMaybe) :. ctx) server <&> \app req respK -> do
      let
        mCookie = (Map.fromList . parseCookies) <$> lookup hCookie (requestHeaders req)
        key = getContextEntry ctx :: Vault.Key (Maybe SessionMap)
        req' = req {vault = Vault.insert key mCookie (vault req)}
      app req' respK

instance
  ( HasServer api ctx
  , HasContextEntry ctx HasCookies
  , HasContextEntry ctx (Vault.Key SessionMap)
  ) =>
  HasServer (WithCookies '[Required] :> api) ctx
  where
  type ServerT (WithCookies '[Required] :> api) m = SessionMap -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server

  route _ ctx server =
    route (Proxy @api) ctx $
      server `addHeaderCheck` retrieveCookies
    where
      retrieveCookies :: DelayedIO SessionMap
      retrieveCookies = withRequest $ \req -> do
        let key = getContextEntry ctx :: Vault.Key SessionMap
        case Vault.lookup key (vault req) of
          Just cookies -> pure cookies
          Nothing ->
            delayedFailFatal $
              err500
                { errBody = "Something has gone horribly wrong; could not find cached cookies."
                }

instance
  ( HasServer api ctx
  , HasContextEntry ctx (HasCookiesMaybe)
  , HasContextEntry ctx (Vault.Key (Maybe SessionMap))
  ) =>
  HasServer (WithCookies '[Optional] :> api) ctx
  where
  type ServerT (WithCookies '[Optional] :> api) m = Maybe SessionMap -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server

  route _ ctx server =
    route (Proxy @api) ctx $
      server `addHeaderCheck` retrieveCookies
    where
      retrieveCookies :: DelayedIO (Maybe SessionMap)
      retrieveCookies = withRequest $ \req -> do
        let key = getContextEntry ctx :: Vault.Key (Maybe SessionMap)
        case Vault.lookup key (vault req) of
          Just cookies -> pure cookies
          Nothing ->
            delayedFailFatal $
              err500
                { errBody = "Something has gone horribly wrong; could not find cached cookies."
                }

updateCookies :: Key -> SessionMap -> SetCookie -> a -> IO (Headers '[Servant.Header "SetCookie" SetCookie] a)
-- updateCookies :: _
updateCookies cookieEncryptKey sessionMap setCookieDefaults value = do
  -- let newCookies = newMap `Map.difference` oldMap
  --     changedCookies = Map.filterWithKey (checkIfMapValueChanged oldMap) oldMap
  --     setCookieList = fmap snd  $ Map.toList $ Map.mapWithKey (keyValueToSetCookie setCookieDefaults) changedCookies
  let
    sessionMapOfText :: Map Text Text
    sessionMapOfText = Map.fromList $ fmap (\(k, v) -> (decodeUtf8 k, decodeUtf8 v)) $ Map.toList sessionMap
    sessionMapBS :: ByteString
    sessionMapBS = LBS.toStrict $ Aeson.encode sessionMapOfText
  sessionMapEncrypted <- encryptIO cookieEncryptKey sessionMapBS
  let
    setCookie =
      setCookieDefaults
        { setCookieName = "something intelligent here"
        , setCookieValue = sessionMapEncrypted
        }

  pure $ addHeader setCookie value

clearSession :: SetCookie -> a -> IO (Headers '[Servant.Header "SetCookie" SetCookie] a)
clearSession setCookieDefaults value = do
  let
    myEmptyMap :: Map Text Text
    myEmptyMap = Map.empty

    emptyObjectBS = LBS.toStrict $ Aeson.encode myEmptyMap

    setCookie =
      setCookieDefaults
        { setCookieName = "something intelligent here"
        , setCookieValue = emptyObjectBS
        }
  pure $ addHeader setCookie value
