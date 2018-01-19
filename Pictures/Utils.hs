{-# LANGUAGE OverloadedStrings #-}

module W7W.Pictures.Utils where

import Hakyll

import W7W.Utils

-- picturesPattern :: Item a -> Pattern
-- picturesPattern i =
--   basicPicturesPattern i "*"

-- basicImagePattern :: Item a -> String -> Pattern
-- basicImagePattern i p =
--   fromGlob $ "images/" ++ (itemYear i) ++ "/" ++ (itemCanonicalName i) ++ "/" ++ p

-- basicImageUrl :: Item a -> String
-- basicImageUrl i = "/images/" ++ (itemYear i) ++ "/" ++ (itemCanonicalName i) ++ "/"

hasPictures ::  (Item a -> Pattern) -> Item a -> Compiler Bool
hasPictures pPattern i = do
  is <- getMatches (pPattern i)
  return . not . null $ is

loadPictures :: Pattern -> Compiler [Item String]
loadPictures pPattern = do
  ps <- getMatches pPattern
  return $ map (\p -> Item {itemIdentifier = p, itemBody = ""}) ps
