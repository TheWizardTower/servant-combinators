{- |Description: Provides a combinator to access the Query String in
 its raw form, from the WAI request.
-}
module Servant.API.RawQueryString where

import Data.ByteString (ByteString)
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

{- |
  @RawQueryString@ gives handler authors a combinator to access the raw
  (that is, un-parsed) query string from the WAI request, as a
  ByteString.

  Generally speaking, you should prefer to use the @QueryString@
  combinator, but if you need access to the raw value, this combinator
  provides it.

  Example:

@
import Control.Monad.IO.Class (liftIO)
import Servant
import ServantExtras.RawQueryString

import qualified Network.HTTP.Types.Header as NTH (Header)

type MyAPI = "my-query-endpoint"
           :> RawQueryString
           :> Get '[JSON] NoContent

myServer :: Server MyAPI
myServer = queryEndpointHandler
  where
    queryEndpointHandler :: ByteString -> Handler NoContent
    queryEndpointHandler queryStr =
      -- do something with the ByteString, like pass it to a
      -- sub-process

@
-}
data RawQueryString

instance HasServer api ctx => HasServer (RawQueryString :> api) ctx where
  type ServerT (RawQueryString :> api) m = ByteString -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server
  route _ ctx server =
    route (Proxy @api) ctx $
      server `passToServer` \req ->
        rawQueryString req
