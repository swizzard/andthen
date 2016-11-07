{-# LANGUAGE OverloadedStrings #-}
module TwitterClient where

import Web.Twitter.Conduit hiding (map, count)
import Web.Twitter.Conduit.Parameters (count)
import Web.Twitter.Types
import qualified Data.Text as T
import Data.Function ((&))
import Control.Lens ((?~))

import Conf (readCreds)

creds :: IO TWInfo
creds = readCreds "/Users/samuelraker/haskell-stuff/AndThen/creds.json"

mgr :: IO Manager
mgr = newManager tlsManagerSettings

getTexts :: (SearchResult [Status]) -> [T.Text]
getTexts = searchResultStatuses !: map statusText where
    (!:) = flip (.)

getTweets :: T.Text -> Integer -> IO [T.Text]
getTweets q n = getTexts <$>
                (creds >>= \c ->
                 mgr >>= \m ->
                 call c m $ (search q) & count ?~ n)
