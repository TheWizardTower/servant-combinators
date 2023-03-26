module ServantExtras.Cookies  where

import Network.HTTP.Types.Header
import Web.ClientSession
import Web.Cookie
import Network.Wai
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)
import Servant
import Data.Map.Strict (Map)
import Servant.Server.Internal.Delayed (passToServer)

import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

type SessionMap = Map ByteString ByteString

data RequestCookies

instance HasServer api ctx => HasServer (RequestCookies :> api) ctx
  where
    type ServerT (RequestCookies :> api) m = SessionMap -> ServerT api m

    hoistServerWithContext _ ctx nt server =
      hoistServerWithContext (Proxy @api) ctx nt . server
    route _ ctx server = route (Proxy @api) ctx $
      server `passToServer` \req ->
        let mCookie = lookup hCookie (requestHeaders req)
         in maybe Map.empty (Map.fromList . parseCookies) mCookie

updateCookies :: Key -> SessionMap -> SetCookie -> a -> IO (Headers '[Servant.Header  "SetCookie" SetCookie] a)
-- updateCookies :: _
updateCookies cookieEncryptKey sessionMap setCookieDefaults value = do
  -- let newCookies = newMap `Map.difference` oldMap
  --     changedCookies = Map.filterWithKey (checkIfMapValueChanged oldMap) oldMap
  --     setCookieList = fmap snd  $ Map.toList $ Map.mapWithKey (keyValueToSetCookie setCookieDefaults) changedCookies
  let
    sessionMapOfText :: Map Text Text
    sessionMapOfText = Map.fromList $ fmap (\(k,v) -> (decodeUtf8 k, decodeUtf8 v)) $ Map.toList sessionMap
    sessionMapBS :: ByteString
    sessionMapBS = LBS.toStrict $ Aeson.encode sessionMapOfText
  sessionMapEncrypted <- encryptIO cookieEncryptKey sessionMapBS
  let
    setCookie = setCookieDefaults
      { setCookieName = "something intelligent here"
      , setCookieValue = sessionMapEncrypted }

  pure $ addHeader setCookie value

clearSession :: SetCookie -> a -> IO (Headers '[Servant.Header  "SetCookie" SetCookie] a)
clearSession setCookieDefaults value = do
  let
    myEmptyMap :: Map Text Text
    myEmptyMap = Map.empty

    emptyObjectBS = LBS.toStrict $ Aeson.encode myEmptyMap

    setCookie = setCookieDefaults
      { setCookieName = "something intelligent here"
      , setCookieValue = emptyObjectBS
      }
  pure $ addHeader setCookie value
