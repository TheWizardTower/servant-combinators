{- |Description: Combinator for Servant to allow Handlers access to
  the raw path from the WAI request.
-}
module Servant.API.PathInfo where

import Data.Text (Text)
import Network.HTTP.Types (decodePathSegments)
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

{- |
  @PathInfo@ provides handlers access to the path segments from the
  request, without the domain name or query parameters. We re-generate
  this from the rawPathInfo via
  @Network.HTTP.Types.decodePathSegments@ because Servant removes all
  fields from the @pathInfo@ field of a request as part of routing the
  request to the appropriate handler.

  Example:

@
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)
import Servant
import ServantExtras.RawPathInfo

type MyAPI = "merlin" :> "my-path-info-endpoint"
           :> PathInfo
           :> Get '[JSON] NoContent

myServer :: Server MyAPI
myServer = pathInfoEndpointHandler
 where
   pathInfoEndpointHandler :: [Text] -> Handler NoContent
   pathInfoEndpointHandler pInfo = do
     case (elem "merlin" pInfo) of
      False -> do
        liftIO $ print "This example has a bug!"
        throwError err400 { errBody = "Patches accepted!" }
      True -> do
        liftIO $ print "Hopefully this demonstrates how path info works."
        pure NoContent
@
-}
data PathInfo

instance HasServer api ctx => HasServer (PathInfo :> api) ctx where
  type ServerT (PathInfo :> api) m = [Text] -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server
  route _ ctx server =
    route (Proxy @api) ctx $
      server `passToServer` \req ->
        decodePathSegments $ rawPathInfo req
