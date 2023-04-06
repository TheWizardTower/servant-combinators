-- |Description: Combinator for Servant to allow Handlers access to the full query
-- string from the WAI request.
module ServantExtras.QueryString where

import Network.HTTP.Types (Query)
import Network.Wai
import Servant
import Servant.Server.Internal.Delayed (passToServer)

{-|
  @QueryString@ provides handlers access to the full query string from
  the WAI request, rather than pulling each element explicitly. This
  allows for dynamic query management, or to simply take in many
  queries in one argument.

  Example:

@
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types (Query, renderQuery)
import Servant
import ServantExtras.QueryString

type MyAPI = "my-cookie-enabled-endpoint"
           :> QueryString
           :> Get '[JSON] NoContent

myServer :: Server MyAPI
myServer = queryEndpointHandler
 where
   queryEndpointHandler :: Query -> Handler NoContent
   queryEndpointHandler query = do
    liftIO $ print $ renderQuery True query
    let mCookieValue = lookup "merlinWasHere" query in
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
data QueryString

instance HasServer api ctx => HasServer (QueryString :> api) ctx where
  type ServerT (QueryString :> api) m = Query -> ServerT api m

  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy @api) ctx nt . server
  route _ ctx server =
    route (Proxy @api) ctx $
      server `passToServer` \req ->
        queryString req
