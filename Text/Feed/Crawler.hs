{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Text.Feed.Crawler where
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment
import System.IO
import Control.Monad.Trans.State.Lazy 
import Control.Monad.Trans.Class (lift)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header 
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)

type Location = B.ByteString

withRedirectTracking :: ManagerSettings 
                     -> Request 
                     -> IO (Response BL.ByteString, [Location])
withRedirectTracking settings request = do
    m <- newManager settings
    r <- myHttp request m
    return r

myHttp :: Request -> Manager -> IO (Response BL.ByteString, [Location])
myHttp req man = do
   (res, locations) <- runStateT (traceRedirects req man) []
   return (res, locations)

traceRedirects :: Request -> Manager -> StateT [Location] IO (Response BL.ByteString)
traceRedirects req' man = do
   let req = req' { checkStatus = \_ _ _ -> Nothing }
   res <- httpLbs req{redirectCount=0} man
   let req2 = getRedirectedRequest req (responseHeaders res) (responseCookieJar res) (statusCode (responseStatus res))
   let location = lookup hLocation . responseHeaders $ res
   case (req2, location) of 
      (Just req2', Just location') -> do
          let location = lookup hLocation . responseHeaders $ res
          modify (\locations -> location' : locations)
          traceRedirects req2' man
      _ -> return res

