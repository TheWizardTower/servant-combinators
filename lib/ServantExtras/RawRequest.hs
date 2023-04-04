module ServantExtras.RawRequest where

import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

data RawRequest

instance HasServer api ctx => HasServer (RawRequest :> api) ctx where
  type ServerT (RawRequest :> api) m = Request -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server
  route _ ctx server =
    route (Proxy @api) ctx $
      server `passToServer` \req ->
        req
