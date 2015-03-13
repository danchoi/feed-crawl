{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
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
import Text.Feed.Crawler

import qualified Control.Exception as E


main :: IO ()
main = do
    url:_ <- getArgs
    request <- parseUrl url
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    res  <- (Just `fmap` (withRedirectTracking settings request))
        `E.catch` (\e -> do
                putStrLn "Exception caught"
                print (e :: HttpException)
                return Nothing
            )
    case res of 
      Just (resp, xs) -> do
          print xs
          print . lookup hContentType . responseHeaders $ resp
      Nothing -> 
          putStrLn "No result"


-- https://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Exception.html#g:3
