module ServantExtras.HeaderList where

import Network.HTTP.Types.Header
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

data HeaderList

instance HasServer api ctx => HasServer (HeaderList :> api) ctx where
  type ServerT (HeaderList :> api) m = [Network.HTTP.Types.Header.Header] -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server
  route _ ctx server =
    route (Proxy @api) ctx $
      server `passToServer` \req ->
        requestHeaders req
