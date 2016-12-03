module Util where

import qualified Data.Text as T
import Data.Text.ICU.Regex


(?>) :: T.Text -> T.Text -> IO (Bool, Regex)
p ?> s = do
    r <- regex [CaseInsensitive] p
    setText r s
    m <- find r $ -1
    return (m, r)

quoteWrap :: T.Text -> T.Text
quoteWrap = (T.cons '"') . (`T.snoc` '"')

