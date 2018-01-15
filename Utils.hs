{-# LANGUAGE OverloadedStrings #-}
module W7W.Utils where

import System.FilePath.Posix ((</>), takeBaseName)


import Hakyll

itemYear :: Item a -> String
itemYear = flip (!!) 1 . itemPathParts

itemLang :: Item a -> String
itemLang = head . itemPathParts

itemCanonicalName :: Item a -> String
itemCanonicalName = identifierCanonicalName . itemIdentifier

identifierCanonicalName :: Identifier -> String
identifierCanonicalName = getCanonicalName . reverse . identifierPathParts
  where
    getCanonicalName =  takeBaseName . head

itemPathParts :: Item a -> [String]
itemPathParts = identifierPathParts . itemIdentifier

identifierPathParts :: Identifier -> [String]
identifierPathParts i = splitAll "/" (toFilePath i)

loadImages :: Pattern -> Compiler [Item CopyFile]
loadImages = loadAll

--
--
-- template utils
--
--
applyTemplateSnapshot tplPattern cx i = do
  t <- loadSnapshotBody tplPattern "template"
  applyTemplate t cx i

applyTemplateSnapshotList tplPattern cx is = do
  t <- loadSnapshotBody tplPattern "template"
  applyTemplateList t cx is
