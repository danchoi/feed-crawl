{-# LANGUAGE OverloadedStrings, RecordWildCards, Arrows #-} 
module Text.Feed.Crawl.DetectLink where
import Text.XML.HXT.Core 

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
      deep (isElem 
        >>> hasName "link" 
        >>> hasAttrValue "rel" (== "alternate")
        >>> (getAttrValue "href"
              &&& getAttrValue "type")
      )

