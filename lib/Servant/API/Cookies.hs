{-# LANGUAGE FlexibleContexts #-}

{- |Description: This module provides access to cookie data, in the
 form of a SessionMap.
-}
module Servant.API.Cookies where

import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
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
import qualified Network.HTTP.Types.Header as NTH

-- |A SessionMap is a hash map of session data from a request.
type SessionMap = Map ByteString ByteString

{- |
  The @ProvideCookies@ and @WithCookies@ combinator work in tandem
  together -- the @ProvideCookies@ combinator parses the cookies from
  the request and stores them in the WAI request Vault, the
  @WithCookies@ combinator provides the cookies as a hash map to the
  handler.
-}
data ProvideCookies (mods :: [Type])

{- |
  As mentioned above, the @WithCookies@ combinator provides
  already-parsed cookies to the handler as a SessionMap.

  A potentially relevant note: the cookie data is sent to the client
  _unencrypted_. If you need to encrypt your data, you'll need to use
  a different combinator.

  Example:

@
import Control.Monad.IO.Class (liftIO)
import Servant
import ServantExtras.Cookies

import qualified Data.Map.Strict as Map

type MyAPI = "my-cookie-enabled-endpoint"
           :> ProvideCookies '[Required]
           :> WithCookies '[Required]
           :> Get '[JSON] NoContent

myServer :: Server MyAPI
myServer = cookieEndpointHandler
 where
   cookieEndpointHandler :: SessionMap -> Handler NoContent
   cookieEndpointHandler sMap =
      let mCookieValue = lookup "MerlinWasHere" $ Map.toList sMap in
      case mCookieValue of
       Nothing -> do
         liftIO $ print "Merlin was *NOT* here!"
         throwError err400 { errBody = "Clearly you've missed something." }
       Just message -> do
         liftIO $ do
           print "Merlin WAS here, and he left us a message!"
           print message
         pure NoContent
@
-}
data WithCookies (mods :: [Type])

{- |
  @HasCookies@ and @HasCookiesMaybe@ are internal utitily types. You should only need to use @ProvideCookies@ and @WithCookies@.

  As an aside, they're separate types (rather than a single type with
  a (mods :: [Type]) ) phantom type because the term-level values show up
  in the instances, and I didn't see a clean way to separate them out
  by case, and only covering one value from the sum type made Haskell
  (rightly) complain.
-}
data HasCookies = HasCookies

{- |
  @HasCookies@ and @HasCookiesMaybe@ are internal utitily types. You should only need to use @ProvideCookies@ and @WithCookies@.
-}
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
        mCookie = lookup NTH.hCookie (requestHeaders req)
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
        mCookie = (Map.fromList . parseCookies) <$> lookup NTH.hCookie (requestHeaders req)
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
                { -- TODO: Maybe the error message should be pulled from
                  -- the Context?
                  errBody = "Something has gone horribly wrong; could not find cached cookies."
                }

{- |
  This function takes a SessionMap and provides a "Set-Cookie" header
  to set the SessionData to a newly minted value of your choice.
-}
updateCookies :: Key -> SessionMap -> SetCookie -> a -> IO (Headers '[Servant.Header "Set-Cookie" SetCookie] a)
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

{- |
  This function clears session data, for a fresh, minty-clean
  experience. The archetypal use case is when a user logs out from
  your server.
-}
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
