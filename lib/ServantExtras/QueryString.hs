module ServantExtras.QueryString where

import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

data QueryString

instance HasServer api ctx => HasServer (QueryString :> api) ctx
  where
    type ServerT (QueryString :> api) m = Query -> ServerT api m

    hoistServerWithContext _ ctx nt server =
      hoistServerWithContext (Proxy @api) ctx nt . server
    route _ ctx server = route (Proxy @api) ctx $
      server `passToServer` \req ->
        queryString req
