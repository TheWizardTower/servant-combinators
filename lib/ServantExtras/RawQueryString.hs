module ServantExtras.RawQueryString where

import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)
import Data.ByteString (ByteString)

data RawQueryString

instance HasServer api ctx => HasServer (RawQueryString :> api) ctx
  where
    type ServerT (RawQueryString :> api) m = ByteString -> ServerT api m

    hoistServerWithContext _ ctx nt server =
      hoistServerWithContext (Proxy @api) ctx nt . server
    route _ ctx server = route (Proxy @api) ctx $
      server `passToServer` \req ->
        rawQueryString req
