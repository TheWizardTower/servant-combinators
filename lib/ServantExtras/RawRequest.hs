-- |Description: Provide a combinator to give handlers access to the
-- raw WAI request.
module ServantExtras.RawRequest where

import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

{-|
  @RawRequest@ provides the `Network.Wai.Request` field from the WAI request.

  Example:

@
import Control.Monad.IO.Class (liftIO)
import Network.Wai
import Servant
import ServantExtras.RawRequest

type MyAPI = "my-request-endpoint"
           :> RawRequest
           :> Get '[JSON] NoContent

myServer :: Server MyAPI
myServer = requestEndpointHandler
  where
    requestEndpointHandler :: Request -> Handler NoContent
    requestEndpointHandler req =
      -- Do something clever with the request
      pure NoContent
@
-}
data RawRequest

instance HasServer api ctx => HasServer (RawRequest :> api) ctx where
  type ServerT (RawRequest :> api) m = Request -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server
  route _ ctx server =
    route (Proxy @api) ctx $
      server `passToServer` \req ->
        req
