{-# LANGUAGE OverloadedStrings #-}

module W7W.Typography
  (beautifyTypography)
where

import qualified Data.Text as T

import Hakyll

beautifyDashes :: T.Text -> T.Text
beautifyDashes = (T.replace " - " "&nbsp;&mdash; ") . (T.replace " — " "&nbsp;&mdash; ")

beautifyTypography :: Item String -> Compiler (Item String)
beautifyTypography = withItemBody beautify
  where
    beautify = return . T.unpack . beautifyDashes . T.pack
