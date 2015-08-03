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
import Network.URI
import Control.Applicative 
import Data.Monoid
import Control.Monad (join)


test :: String -> IO ()
test url = do
    r <- crawlURL url 
    print r


-- | Spoof a Safari Browser because some sites don't even serve feeds to 
--   an http-conduit client
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.75.14 (KHTML, like Gecko) Version/7.0.3 Safari/7046A194A"

-- |The main function
crawlURL :: String 
          -> IO CrawlResult
crawlURL url = do
    request <- parseUrl url
    let request' = request {
          requestHeaders = ("User-Agent", userAgent):(requestHeaders request)
        }
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    (mkCrawlResult url =<< withRedirectTracking settings request')
      `E.catch` (\e -> return . Left . CrawlHttpError $ e)
    

mkCrawlResult :: String -> (Response BL.ByteString, [Status]) -> IO CrawlResult
mkCrawlResult firstUrl (resp, statuses) = do
  let ct = lookup hContentType . responseHeaders $ resp
  let loc = lookup hLocation . responseHeaders $ resp
  let urls = catMaybes [ sLocation | Status{..}  <- statuses ]
  let lastUrl = head (urls ++ [B.pack firstUrl])
  if isFeedContentType ct 
      then return . Right $ CrawlSuccess {
                    crawlLastContentType = ct
                  , crawlLastUrl = lastUrl
                  , crawlFeedContent = responseBody resp
                  }
      else do
          links <- findFeedLinks (BL.unpack . responseBody $ resp)
          -- TODO this ignore any the <base> tag in the html page
          return . Left . CrawlFoundFeedLinks (responseBody resp)
                 . map (ensureLinkIsAbsolute lastUrl) $ links
  where ensureLinkIsAbsolute :: B.ByteString -> Link -> Link
        ensureLinkIsAbsolute linkBaseUrl x@Link{..} = 
            let linkHref' = 
                  maybe linkHref id
                    $ ensureAbsURL' 
                        (parseURI . B.unpack $ linkBaseUrl) 
                        linkHref
            in x { linkHref = linkHref' }

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
          let location' = ensureAbsURL req location
          let st = Status {
              sStatusCode = statusCode (responseStatus res)
            , sLocation = location' 
            , sContentType = lookup hContentType . responseHeaders $ res
            }
          modify (st:)
          traceRedirects req2' man
      _ -> return res

isFeed :: Status -> Bool
isFeed Status{..} = isFeedContentType sContentType 

ensureAbsURL :: Request -> Maybe B.ByteString -> Maybe B.ByteString
ensureAbsURL req url = 
    let sUrl = B.unpack `fmap` url
    in case sUrl of
        Nothing    -> Nothing
        Just sUrl' -> B.pack <$> ensureAbsURL' (baseURL req) sUrl'

ensureAbsURL' :: Maybe URI -> String -> Maybe String
ensureAbsURL' baseURI s = 
    case parseURI s of
      Just _  -> Just s   -- is a valid full URI
      Nothing ->     -- url may be relative path, join to request info
        (\x -> uriToString id x "") 
          <$> (nonStrictRelativeTo 
                <$> (parseRelativeReference s)
                <*> baseURI)

baseURL :: Request -> Maybe URI
baseURL req = 
    let protocol = if secure req then "https://" else "http://"
    in parseURI $ mconcat [
                    protocol
                  , B.unpack . host $ req
                  , case port req of 
                      80 -> "" 
                      n -> ":" ++ show n
                  ]

