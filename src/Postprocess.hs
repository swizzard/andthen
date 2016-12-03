{-# LANGUAGE OverloadedStrings #-}
module Postprocess where

import Control.Monad ((>=>), join)
import qualified Data.Text as T
import qualified Data.Vector as V
import NLP.Tokenize.Text (whitespace, finalPunctuation, run)

import Util ((?>))

data Token = Start { token :: T.Text } |
             Neither { token :: T.Text } |
             End { token :: T.Text }
             deriving (Eq, Show)

dummyToken :: IO Token
dummyToken = return $ End ""

toText :: V.Vector Token -> T.Text
toText = (T.concat . V.toList . V.map token)

tokenize :: T.Text -> V.Vector T.Text
tokenize txt = V.fromList $ run (whitespace >=> finalPunctuation) txt

parseToken :: IO Token -> T.Text -> IO Token
parseToken t s = t >>= parseToken' s where
    parseToken' s' (End _) = return $ Start s'
    parseToken' s' _ = let endPunc = "(\\.|\\?|!|\"$|\'$)+" in
                       (endPunc ?> s') >>= \(m, _) -> 
                       return $ if m then End s' else Neither s'

parseTokens :: T.Text -> IO (V.Vector Token)
parseTokens t = V.sequence $
                (flip V.snoc) dummyToken $
                V.scanl' parseToken dummyToken $
                tokenize t

parseTweets :: V.Vector T.Text -> IO (V.Vector Token)
parseTweets ts = V.mapM parseTokens ts >>= (return . join)
