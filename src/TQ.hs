{-# LANGUAGE OverloadedStrings #-}
module TQ where

import Data.Default
import qualified Data.Text as T
import Data.Text.ICU.Regex

import QHelper
import Postprocess
import TwitterClient

data TQ = TQ { qs :: T.Text, qo :: QOptions, p :: IO Regex }

firstTQ :: TQ
firstTQ = TQ "first " qoptions  


?:> :: T.Text -> T.Text -> IO Regex
p ?:> s = do
    pat <- regex [CaseInsensitive] p
    setText pat s
    _ <- find pat $ -1
    return pat

addProns :: T.Text -> T.Text
addProns = (flip T.append) " (i|you|he|she|it|we|they)"

scrapeT :: TQ -> Integer -> IO [T.Text]
scrapeT (TQ q _) n = getTweets (buildQ qoptions $ 

scrapeTexts :: [T.Text] -> Integer -> IO [T.Text]
scrape qs n = getTweets (buildQ qoptions $ addPronouns qs) n
