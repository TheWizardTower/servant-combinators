-- |Description: This module provides a way to get _all_ the headers
-- from a request, rather than asking for them piecemeal.
module ServantExtras.HeaderList where

import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

import qualified Network.HTTP.Types.Header as NTH (Header)

{-|
  The HeaderList combinator provides a list of
  @Network.HTTP.Types.Header.Header@ values from the WAI request.

  Example:

@
import Control.Monad.IO.Class (liftIO)
import Servant
import ServantExtras.HeaderList

import qualified Network.HTTP.Types.Header as NTH (Header)

type MyAPI = "my-header-endpoint"
           :> HeaderList
           :> Get '[JSON] NoContent

myServer :: Server MyAPI
myServer = headerEndpointHandler
 where
   headerEndpointHandler :: [NTH.Header] -> Handler NoContent
   headerEndpointHandler headers =
      let mCookieValue = lookup "merlinWasHere" headers in
      case mCookieValue of
       Nothing -> do
         liftIO $ print "Merlin was *NOT* here!"
         throwError err400 { errBody = "Clearly you've missed something." }
       Just message -> do
         liftIO $ do
           print "Merlin WAS here, and he left us a message!"
           print message
         pure NoContent
@
-}
data HeaderList

instance HasServer api ctx => HasServer (HeaderList :> api) ctx where
  type ServerT (HeaderList :> api) m = [NTH.Header] -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server
  route _ ctx server =
    route (Proxy @api) ctx $
      server `passToServer` \req ->
        requestHeaders req
