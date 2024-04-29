{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module W7W.ExifInfo.Types where

import Data.Text

import W7W.MultiLang


--
-- Title
--
data Title = Title {ruTitle :: Maybe Text
                   ,enTitle :: Maybe Text}
           | NoTitle deriving (Show)

instance Localized Title Text where
  localize _ NoTitle = Nothing
  localize RU t = ruTitle t
  localize EN t = enTitle t
  localize _ _ = Nothing

instance IsLocalized Title where
  isLocalized _ NoTitle = False
  isLocalized UNKNOWN _ = False
  isLocalized RU t = maybe False (const True) . ruTitle $ t
  isLocalized EN t = maybe False (const True) . enTitle $ t

--
-- Creator
--
data Creator = Creator {ruCreator :: Maybe Text
                       ,enCreator :: Maybe Text}
             | NoCreator deriving (Show)

instance Localized Creator Text where
  localize _ NoCreator = Nothing
  localize RU c = ruCreator c
  localize EN c = enCreator c
  localize _ _ = Nothing

instance IsLocalized Creator where
  isLocalized _ NoCreator = False
  isLocalized UNKNOWN _ = False
  isLocalized RU c = maybe False (const True) . ruCreator $ c
  isLocalized EN c = maybe False (const True) . enCreator $ c
--
-- Description
--
data Description = Description {ruDescription :: Maybe Text
                               ,enDescription :: Maybe Text}
                 | NoDescription deriving (Show)

instance Localized Description Text where
  localize _ NoDescription = Nothing
  localize RU d = ruDescription d
  localize EN d = enDescription d
  localize _ _ = Nothing

instance IsLocalized Description where
  isLocalized _ NoDescription = False
  isLocalized UNKNOWN _ = False
  isLocalized RU d = maybe False (const True) . ruDescription $ d
  isLocalized EN d = maybe False (const True) . enDescription $ d

--
-- ExifInfo
--
data ExifInfo = ExifInfo { eiTitle :: Title
                         , eiCreator :: Creator
                         , eiDescription :: Description } deriving (Show)

emptyExifInfo :: ExifInfo
emptyExifInfo = ExifInfo NoTitle NoCreator NoDescription
