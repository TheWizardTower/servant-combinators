module TestLib where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Data.ByteString (ByteString)
import Test.QuickCheck.Monadic (PropertyM (..))
import Test.Tasty

import qualified Network.HTTP.Simple as S

testFunctionGeneric :: (Int -> IO a) -> (Int -> TestTree) -> Int -> IO ()
testFunctionGeneric server tests port = do
  withAsync (server port) $ \serverAsync -> do
    -- Sleep five seconds, to ensure the server can come online.
    threadDelay (5 * 100000)
    defaultMain $ tests port
    cancel serverAsync

success :: S.Response ByteString -> PropertyM IO Bool
success resp = do
  let respCode = S.getResponseStatusCode resp
      respBool = respCode == 200
  pure respBool

returns400 :: S.Response ByteString -> PropertyM IO Bool
returns400 resp = do
  let respCode = S.getResponseStatusCode resp
  pure $ respCode == 400
