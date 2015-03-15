{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Text.Feed.Crawl.Common where
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as B

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

