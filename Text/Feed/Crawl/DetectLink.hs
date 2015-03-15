{-# LANGUAGE OverloadedStrings, RecordWildCards, Arrows #-} 
module Text.Feed.Crawl.DetectLink where
import Text.XML.HXT.Core 
import qualified Data.ByteString.Char8 as B

data Link = Link {
    linkRel :: String
  , linkHref :: String
  , linkType :: String
  } deriving Show

findFeedLinks input = do
  let res = runX (
            readString [
                  withParseHTML yes
                ] input 
            >>>
            scrapePage 
            ) 
  res            

scrapePage = 
      deep (isElem >>> hasName "head") >>>
      deep (isElem 
        >>> hasName "link" 
        >>> hasAttrValue "rel" (== "alternate")
        >>> (proc x -> do
              rel <- getAttrValue "rel" -< x
              href <- getAttrValue "href" -< x
              typ <- getAttrValue "type" -< x
              returnA -< Link rel href typ)
      )

