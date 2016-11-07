{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Conf (readCreds) where

import Control.Monad (liftM)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Char8 (pack)
import GHC.Generics
import Web.Twitter.Conduit


data JCreds = JCreds {
    consumer_key :: String,
    consumer_secret :: String,
    access_token :: String,
    access_token_secret :: String
    } deriving (Generic, Show)

instance FromJSON JCreds

instance ToJSON JCreds where
    toEncoding = genericToEncoding defaultOptions


toOAuth :: JCreds -> TWInfo
toOAuth creds = setCredential t c def where
    t = twitterOAuth { oauthConsumerKey = pack $ consumer_key creds,
                       oauthConsumerSecret = pack $ consumer_secret creds
                     }
    c = Credential [(pack $ access_token creds, pack $ access_token_secret creds)]

readCreds :: String -> IO TWInfo
readCreds fil = do
    res <- liftM ((fmap toOAuth) . decode) $ B.readFile fil
    case res of
        Nothing -> error "can't read creds"
        (Just c) -> return c
