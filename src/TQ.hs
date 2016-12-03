{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- Usage:
-- useTQ $ def & tCount .~ 10
module TQ
    (
      useTQ
    , TQ
    , QOptions
    )
where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Data.Text.Conversions
import Data.Text.ICU.Regex
import Data.Text.Foreign (dropWord16)
import Wordfilter (blacklisted)

import QHelper
import TwitterClient
import Util

data Pronoun = I | You | He | She | It | We | They deriving (Show)

instance ToText Pronoun where
  toText = T.pack . show

data TQ = TQ { _queryString :: T.Text,
               _pronoun :: Maybe Pronoun,
               _tCount :: Integer,
               _pat :: Maybe T.Text }

makeLenses ''TQ

instance Default TQ where
  def = TQ "" Nothing 0 Nothing

addPronoun :: T.Text -> Maybe Pronoun -> T.Text
addPronoun s Nothing = quoteWrap s
addPronoun s (Just pron) = quoteWrap $ T.concat [s, " ", toText pron]

trimTexts :: Maybe T.Text -> [T.Text] -> IO [T.Text]
trimTexts Nothing ts = return (map T.toLower ts)
trimTexts (Just patt) ts = foldM f [] (map T.toLower ts) where
    f l t = do
        (mtch, rgx) <- patt ?> t
        s <- start rgx 0
        return $ if mtch then
            maybeToList (flip dropWord16 t <$> s) ++ l
            else l

blFilter :: [T.Text] -> IO [T.Text]
blFilter = filterM (fmap not . blacklisted . T.unpack)

useTQ :: TQ -> IO [T.Text]
useTQ (TQ qs pr tc p) = getTweets (addPronoun qs pr) tc >>=
                        trimTexts p >>=
                        blFilter

