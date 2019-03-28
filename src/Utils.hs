{-# LANGUAGE OverloadedStrings #-}
module W7W.Utils where

import System.FilePath.Posix ((</>), takeBaseName, joinPath)
import Data.List (tail)


import Hakyll

--
-- FIXME: move to WHPH engine!!!!
--
itemYear :: Item a -> String
itemYear = flip (!!) 1 . itemPathParts

itemLang :: Item a -> String
itemLang = head . itemPathParts

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
