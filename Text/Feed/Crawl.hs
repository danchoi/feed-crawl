{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Text.Feed.Crawl where
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


-- |Returns a tuple of response and list of redirect locations. 
--  The first location is the last redirect.
withRedirectTracking :: ManagerSettings 
                     -> Request 
                     -> IO (Response BL.ByteString, [Status'])
withRedirectTracking settings request = do
    m <- newManager settings
    r <- runStateT (traceRedirects request m) []
    return r

data Status' = Status' {
      sStatusCode :: Int
    , sLocation :: Maybe B.ByteString
    , sContentType :: Maybe B.ByteString
    } deriving Show

traceRedirects :: Request 
               -> Manager 
               -> StateT [Status'] IO (Response BL.ByteString)
traceRedirects req' man = do
   let req = req' { checkStatus = \_ _ _ -> Nothing }
   res <- httpLbs req{redirectCount=0} man
   let req2 = getRedirectedRequest req (responseHeaders res) (responseCookieJar res) (statusCode (responseStatus res))
   let location = lookup hLocation . responseHeaders $ res
   case (req2, location) of 
      (Just req2', Just location') -> do
          let st = Status' {
              sStatusCode = statusCode (responseStatus res)
            , sLocation = lookup hLocation . responseHeaders $ res
            , sContentType = lookup hContentType . responseHeaders $ res
            }
          modify (st:)
          traceRedirects req2' man
      _ -> return res

