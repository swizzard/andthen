{-# LANGUAGE OverloadedStrings #-}
module Postprocess where

import qualified Data.Text as T
import Data.Text.ICU.Regex


thenPat :: T.Text
thenPat = 

firstPat :: T.Text
firstPat = addProns "(?!at )first"
