{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module W7W.Pictures.Rules where

import Data.Monoid ((<>))
import qualified Data.Text as T

import System.FilePath

import Hakyll

import W7W.Utils
import W7W.Context
import W7W.Pictures.Utils
-- import W7W.ExifInfo.Types
-- import W7W.ExifInfo
-- import W7W.MultiLang (localize, itemLocale, isLocalized, Localized, IsLocalized)


picturesRules :: (Int, Int) -> Pattern -> Rules ()
picturesRules (w, h) p = do
  match p $ do
    route (customRoute pictureRoute)
    let cmd = "convert"
    let args = [ "-"
                   , "-resize"
                   , concat [show w, "x", show h, "^"]
                   -- , "-gravity"
                   -- , "Center"
                   -- , "-crop"
                   -- , concat [show h, "x", show h, "+0+0"]
                   -- , "+repage"
                   , "-quality"
                   , "79"
                   , "-" ]
    compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)
  where
    pictureRoute i =
      let path = toFilePath i
      in (replaceExtension path) "jpg"
