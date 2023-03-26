module ServantExtras.Path where

import Network.Wai
import Data.Text (Text)
import Servant
import Servant.Server.Internal.Delayed (passToServer)

data PathInfo

instance HasServer api ctx => HasServer (PathInfo :> api) ctx
  where
    type ServerT (PathInfo :> api) m = [Text] -> ServerT api m

    hoistServerWithContext _ ctx nt server =
      hoistServerWithContext (Proxy @api) ctx nt . server
    route _ ctx server = route (Proxy @api) ctx $
      server `passToServer` \req ->
        pathInfo req
