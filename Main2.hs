{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import Text.Feed.Crawl.DetectLink

main :: IO ()
main = do
    s <- getContents 
    res <- findFeedLinks s
    mapM_ print res
