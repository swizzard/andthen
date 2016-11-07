{-# LANGUAGE OverloadedStrings #-}
module QHelper (buildQ, QOptions(), qoptions, addPronouns) where

import Data.Default
import qualified Data.Text as T
import Data.Text.Conversions


quoteWrap :: T.Text -> T.Text
quoteWrap = (T.cons '"') . (`T.snoc` '"')

showq :: Maybe Bool -> T.Text -> [T.Text]
showq Nothing _ = []
showq (Just True) t = [t]
showq (Just False) t = [T.append "-" t]

data QOptions = QOptions {
                safe :: Maybe Bool,
                media :: Maybe Bool,
                links :: Maybe Bool
                }

instance Default QOptions where
    def = QOptions (Just True) (Just False) (Just False)

instance ToText QOptions where
    toText (QOptions s m l) =  T.intercalate " " $ (showq s "filter:safe") ++
                                                   (showq m "filter:media") ++
                                                   (showq l "filter:links")

instance Show QOptions where
    show = (show . toText)

qoptions :: QOptions
qoptions = def
    
buildQ :: QOptions -> [T.Text] -> T.Text
buildQ qo qs = T.concat [(T.intercalate " OR " $ map quoteWrap qs),
                         " ", (toText qo)]

addPronouns :: [T.Text] -> [T.Text]
addPronouns = concatMap addP where
    addP q = zipWith T.append (repeat q)
             ["i", "you", "he", "she",
              "it", "we", "they"]
