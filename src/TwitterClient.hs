{-# LANGUAGE OverloadedStrings #-}
module TwitterClient where

import Control.Lens ((?~))
import Control.Monad (ap, join, liftM3)
import qualified Data.Text as T
import Data.Function ((&))
import Web.Twitter.Conduit hiding (map, count)
import Web.Twitter.Conduit.Parameters (count)
import Web.Twitter.Types

import Conf (readCreds)

creds :: IO TWInfo
creds = readCreds "/Users/samuelraker/haskell-stuff/AndThen/creds.json"

mgr :: IO Manager
mgr = newManager tlsManagerSettings

getTexts :: SearchResult [Status] -> [T.Text]
getTexts = map statusText . searchResultStatuses

getTweets :: T.Text -> Integer -> IO [T.Text]
getTweets q n =  getTexts <$> do
                    c <- creds
                    m <- mgr
                    call c m (search q & count ?~ n)

