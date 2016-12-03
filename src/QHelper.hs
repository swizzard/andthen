{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module QHelper
  (
    buildQ
  , QOptions
  , safe
  , media
  , links
  , retweets
  ) where

import Control.Lens
import Data.Default
import qualified Data.Text as T
import Data.Text.Conversions

import Util (quoteWrap)


showq :: T.Text -> Maybe Bool -> [T.Text]
showq _ Nothing = []
showq t (Just True) = [t]
showq t (Just False) = [T.append "-" t]

data QOptions = QOptions {
                _safe :: Maybe Bool,
                _media :: Maybe Bool,
                _links :: Maybe Bool,
                _retweets :: Maybe Bool
                }

makeLenses ''QOptions

instance Default QOptions where
    def = QOptions (Just True) (Just False) (Just False) (Just False)

instance ToText QOptions where
    toText qo =  T.intercalate " " $
                 (showq "filter:safe" $ qo ^. safe) ++
                 (showq "filter:media" $ qo ^. media) ++
                 (showq "filter:links" $ qo ^. links) ++
                 (showq "filter:nativeretweets" $ qo ^. retweets)

instance Show QOptions where
    show = (show . toText)

qoptions :: QOptions
qoptions = def

buildQ' :: QOptions -> [T.Text] -> T.Text
buildQ' qo qs = (T.intercalate " OR " $ map quoteWrap qs) `T.append` " " `T.append` (toText qo)

buildQ :: [T.Text] -> T.Text
buildQ = buildQ' qoptions

