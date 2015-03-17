{-# LANGUAGE OverloadedStrings, RecordWildCards, Arrows #-} 
module Text.Feed.Crawl.DetectLink where
import Text.XML.HXT.Core 
import qualified Data.ByteString.Char8 as B
import Text.Feed.Crawl.Common

findFeedLinks :: String -> IO [Link]
findFeedLinks input = do
  runX (
      readString [
            withParseHTML yes
          ] input 
      >>>
      scrapePage ) 

-- TODO fix relative links
scrapePage = 
      deep (isElem >>> hasName "head") >>>
      deep (isElem 
        >>> hasName "link" 
        >>> hasAttrValue "rel" (== "alternate")
        >>> (proc x -> do
              rel <- getAttrValue "rel" -< x
              href <- getAttrValue "href" -< x
              typ <- getAttrValue "type" -< x
              title <- (getAttrValue "title" `orElse` constA "") -< x
              returnA -< Link rel href typ title)
      )

