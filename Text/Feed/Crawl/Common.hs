{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Text.Feed.Crawl.Common where
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Conduit (HttpException)

type CrawlResult = Either CrawlFail CrawlSuccess

data CrawlFail = 
    CrawlFoundFeedLinks [Link] 
  | CrawlHttpError HttpException
  deriving Show

data CrawlSuccess = CrawlSuccess {
      crawlLastContentType :: Maybe B.ByteString
    , crawlLastUrl :: B.ByteString
    , crawlFeedContent :: BL.ByteString
  } deriving Show

data Status = Status {
      sStatusCode :: Int
    , sLocation :: Maybe B.ByteString
    , sContentType :: Maybe B.ByteString
    } deriving Show

data Link = Link {
    linkRel :: String
  , linkHref :: String
  , linkType :: String
  , linkTitle :: String
  } deriving Show


isFeedContentType :: Maybe B.ByteString -> Bool
isFeedContentType Nothing = False   -- right logic? maybe default to trying to parse unknown type
isFeedContentType (Just bs) = 
    -- e.g. input is "text/html; charset=utf-8" 
    let (mimetype, _) = B.break (== ';') bs 
    in map toLower (B.unpack mimetype) `elem` feedMimeTypes

feedMimeTypes = [
    "application/rss+xml"
  , "application/rdf+xml"
  , "application/atom+xml"
  , "application/xml"
  , "text/xml"]

