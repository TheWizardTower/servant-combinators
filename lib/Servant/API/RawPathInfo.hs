{- |Description: Combinator for Servant to allow Handlers access to
  the raw path from the WAI request.
-}
module Servant.API.RawPathInfo where

import Data.ByteString (ByteString)
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

{- |
  @RawPathInfo@ provides handlers access to the raw, unparsed path
  information the WAI request.

  If you wish to get the path segments, you can either use the
  @PathInfo@ combinator in @Servant.API.PathInfo@ or parse it yourself
  with @Network.HTTP.Types.decodePathSegments@

  Example:

@
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)
import Servant
import ServantExtras.RawPathInfo

type MyAPI = "my-path-info-endpoint"
           :> RawPathInfo
           :> Get '[JSON] NoContent

myServer :: Server MyAPI
myServer = queryEndpointHandler
 where
   queryEndpointHandler :: ByteString -> Handler NoContent
   queryEndpointHandler rawPath = do
     case rawPath of
      "/my-path-info-endpoint" -> do
        liftIO $ print "Servant routed us to the right place!"
        pure NoContent
      _ -> do
        liftIO $ print "My example has a bug!"
        throwError err400 { errBody = "Patches accepted!" }
@
-}
data RawPathInfo

instance HasServer api ctx => HasServer (RawPathInfo :> api) ctx where
  type ServerT (RawPathInfo :> api) m = ByteString -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server
  route _ ctx server =
    route (Proxy @api) ctx $
      server `passToServer` \req ->
        rawPathInfo req
