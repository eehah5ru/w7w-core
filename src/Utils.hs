{-# LANGUAGE OverloadedStrings #-}

module W7W.Utils where

import qualified Text.Read as TR

import System.FilePath.Posix ((</>), takeBaseName, joinPath)
import Data.List (tail)


import Hakyll

--
-- FIXME: move to WHPH engine!!!!
--
itemYear :: Item a -> Maybe String
itemYear = identifierYear . itemIdentifier

identifierYear :: Identifier -> Maybe String
identifierYear = fmap show . mIntYear . idYear'
  where
    mIntYear :: String -> Maybe Int
    mIntYear = TR.readMaybe
    idYear' :: Identifier -> String
    idYear' = flip (!!) 1 . identifierPathParts

hasItemYear :: Item a -> Bool
hasItemYear = maybe False (const True) . itemYear

itemLang :: Item a -> String
itemLang = head . itemPathParts

identifierLang :: Identifier -> String
identifierLang = head . identifierPathParts

itemCanonicalName :: Item a -> String
itemCanonicalName = identifierCanonicalName . itemIdentifier

itemCanonicalPath :: Item a -> String
itemCanonicalPath = joinPath . reverse . tail . reverse . tail . itemPathParts

identifierCanonicalName :: Identifier -> String
identifierCanonicalName = getCanonicalName . reverse . identifierPathParts
  where
    getCanonicalName =  takeBaseName . head

itemPathParts :: Item a -> [String]
itemPathParts = identifierPathParts . itemIdentifier

identifierPathParts :: Identifier -> [String]
identifierPathParts i = splitAll "/" (toFilePath i)

--
--
-- template utils
--
--
applyTemplateSnapshot tplPattern cx i = do
  t <- loadSnapshotBody tplPattern "template"
  applyTemplate t cx i

applyMaybeTemplateSnapshot mTplPattern cx i = do
  case mTplPattern of
    Just tpl -> applyTemplateSnapshot tpl cx i
    _ -> return i

applyTemplateSnapshotList tplPattern cx is = do
  t <- loadSnapshotBody tplPattern "template"
  applyTemplateList t cx is

applyCustomPageTemplateSnapshot :: Context String -> Item String -> Compiler (Item String)
applyCustomPageTemplateSnapshot ctx i = do
  mTpl <- getMetadataField (itemIdentifier i) "template"
  case mTpl of
    Just tpl -> applyTemplateSnapshot (fromFilePath tpl) ctx i
    Nothing -> return i
