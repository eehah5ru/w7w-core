{-# LANGUAGE OverloadedStrings #-}

module W7W.ExifInfo where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as T
import Hakyll

import W7W.ExifInfo.Types
import W7W.ExifInfo.Parser


getExifInfo :: Item a -> Compiler ExifInfo
getExifInfo i = do
  info <- return . T.pack =<< unixFilter "w7w/scripts/picture-exif-info.sh" [(toFilePath . itemIdentifier) i] ""
  return . either e' id . parseOnly parseExifInfo $ info
  where
    e' s = error $ unwords ["getExifInfo: error getting exif data from ", ((toFilePath . itemIdentifier) i), "error: ", s ]
