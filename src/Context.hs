{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module W7W.Context where

import Control.Applicative (Alternative (..))

import Hakyll
import Hakyll.Web.Template.Context

import Data.Monoid ((<>), mempty)
import           Control.Monad               (liftM)
import           Data.Ord                    (comparing)
import           Data.List                   (intersperse, sortBy)

import W7W.MultiLang

import W7W.Utils

import qualified W7W.Cache as Cache
--
--
-- utils
--
--

sortByOrder :: MonadMetadata m => [Item a] -> m [Item a]
sortByOrder =
  sortByM $ order'

  where
    order' = withItemMetadata $ getOrder

    getOrder :: Metadata -> String
    getOrder m =
      maybe "9999" id (lookupString "order" m)

    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                   mapM (\x -> liftM (x,) (f x)) xs

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

field' :: String -> (Item a -> Compiler ContextField) -> Context a
field' key value = Context $ \k _ i ->
    if k == key
        then value i
        else noResult $ "Tried field " ++ key

boolFieldM :: String -> (Item a -> Compiler Bool) -> Context a
boolFieldM name f = field' name $ \i -> do
                      b <- f i
                      if b
                        then return EmptyField                                
                        else noResult $ "Field " ++ name ++ " is false"


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
