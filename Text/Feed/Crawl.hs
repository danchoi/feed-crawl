{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Text.Feed.Crawl where
import Text.Feed.Crawl.Common
import Text.Feed.Crawl.DetectLink
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
import Data.Maybe (listToMaybe, catMaybes)
import qualified Control.Exception as E

-- |The main function
crawlURL :: String 
          -> IO CrawlResult
crawlURL url = do
    request <- parseUrl url
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    (mkCrawlResult url =<< withRedirectTracking settings request)
      `E.catch` (\e -> return . Left . CrawlHttpError $ e)
    

mkCrawlResult :: String -> (Response BL.ByteString, [Status]) -> IO CrawlResult
mkCrawlResult firstUrl (resp, statuses) = do
  let ct = lookup hContentType . responseHeaders $ resp
  let loc = lookup hLocation . responseHeaders $ resp
  let urls = catMaybes [ sLocation | Status{..}  <- statuses ]
  if isFeedContentType ct 
      then return . Right $ CrawlSuccess {
                    crawlLastContentType = ct
                  , crawlLastUrl = head (urls ++ [B.pack firstUrl])
                  , crawlFeedContent = responseBody resp
                  }
      else do
          links <- findFeedLinks (BL.unpack . responseBody $ resp)
          return . Left $ CrawlFoundFeedLinks links
        

-- |Returns a tuple of response and list of redirect locations. 
--  The first location is the last redirect.
withRedirectTracking :: ManagerSettings 
                     -> Request 
                     -> IO (Response BL.ByteString, [Status])
withRedirectTracking settings request = do
    m <- newManager settings
    r <- runStateT (traceRedirects request m) []
    return r

traceRedirects :: Request 
               -> Manager 
               -> StateT [Status] IO (Response BL.ByteString)
traceRedirects req' man = do
   let req = req' { checkStatus = \_ _ _ -> Nothing }
   res <- httpLbs req{redirectCount=0} man
   let req2 = getRedirectedRequest req (responseHeaders res) (responseCookieJar res) (statusCode (responseStatus res))
   let location = lookup hLocation . responseHeaders $ res
   case (req2, location) of 
      (Just req2', Just location') -> do
          let st = Status {
              sStatusCode = statusCode (responseStatus res)
            , sLocation = lookup hLocation . responseHeaders $ res
            , sContentType = lookup hContentType . responseHeaders $ res
            }
          modify (st:)
          traceRedirects req2' man
      _ -> return res

isFeed :: Status -> Bool
isFeed Status{..} = isFeedContentType sContentType 


