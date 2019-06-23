{-# LANGUAGE OverloadedStrings #-}

module W7W.Context where

import Control.Applicative (Alternative (..))

import Hakyll

import Data.Monoid ((<>), mempty)

import W7W.MultiLang

import W7W.Utils

import qualified W7W.Cache as Cache
--
--
-- utils
--
--

hasMetadataField :: String -> Metadata -> Bool
hasMetadataField fName m =
  case lookupString fName m of
    Just _ -> True
    Nothing -> False

hasItemField :: MonadMetadata m => String -> Item a -> m Bool
hasItemField fName =
  withItemMetadata $ hasMetadataField fName


withItemMetadata :: MonadMetadata m => (Metadata -> b) -> Item a -> m b
withItemMetadata f item = do
  m <- getMetadata (itemIdentifier item)
  return $ f m


boolFieldM :: String -> (Item a -> Compiler Bool) -> Context a
boolFieldM name f = field name $ \i -> do
                      b <- f i
                      if b
                        then pure $ error $ unwords $
                                 ["no string value for bool field:",name]
                        else empty


-- TODO: slow version. runs for every item. replace with mkFieldRevision and mkSiteContext
mkFieldRevision :: Cache.Caches -> Compiler (Context a)
mkFieldRevision caches = 
  do
    r <- getRevision'
    return $ field "revision" (return . const r) --(\_ -> return r)
  where
    getCachedRevision = do
      Cache.compilerLookup (Cache.revisionCache caches) "/revision"
    
    cacheRevision rev = do
      Cache.compilerInsert (Cache.revisionCache caches) "/revision" rev
    
    getRevision' = do
     (getCachedRevision) <|> (getRevision >>= cacheRevision)
     
    getRevision = do
     rev <- unixFilter "git" ["rev-parse", "HEAD"] ""
     isDirty <- unixFilter "w7w/scripts/check-repo-is-clean.sh" [] ""
     randomNumber <- unixFilter "w7w/scripts/repo-md5-changes.sh" [] ""
     case isDirty of
       "clean" -> return rev
       "dirty" -> return randomNumber
       _ -> return randomNumber
--
--
-- fields
--
--

fieldCanonicalName = field "canonicalName" $ return . itemCanonicalName

fieldLang = field "lang" $ return . itemLang
