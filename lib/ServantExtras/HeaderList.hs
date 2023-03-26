module ServantExtras.HeaderList where


import Network.HTTP.Types.URI
import Network.HTTP.Types.Header
import Web.Cookie (parseCookies)
import Network.Wai
import Data.Text (Text)
import Data.ByteString (ByteString)
import Servant
import Data.Map.Strict (Map)
import Servant.Server.Internal.Delayed (passToServer)

import qualified Data.Map.Strict as Map

data HeaderList

instance HasServer api ctx => HasServer (HeaderList :> api) ctx
  where
    type ServerT (HeaderList :> api) m = [Network.HTTP.Types.Header.Header] -> ServerT api m

    hoistServerWithContext _ ctx nt server =
      hoistServerWithContext (Proxy @api) ctx nt . server
    route _ ctx server = route (Proxy @api) ctx $
      server `passToServer` \req ->
        requestHeaders req
