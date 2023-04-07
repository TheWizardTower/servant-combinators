-- |Description: Provides a combinator to give handlers access to the
-- PathInfo field from the WAI request.
module ServantExtras.Path where

import Data.Text (Text)
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

{-|
  @PathInfo@ provides the `Network.Wai.pathInfo` field from the WAI request.

  Example:

@
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Servant
import ServantExtras.Path

type MyAPI = "my-path-endpoint"
           :> PathInfo
           :> Get '[JSON] NoContent

myServer :: Server MyAPI
myServer = pathEndpointHandler
  where
    pathEndpointHandler :: [Text] -> Handler NoContent
    pathEndpointHandler pInfo =
      -- Do something clever with the path fragments.
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
        pathInfo req
