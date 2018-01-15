{-# LANGUAGE OverloadedStrings #-}

module W7W.Context where

import Control.Applicative (Alternative (..))

import Hakyll

import Data.Monoid ((<>), mempty)

import W7W.MultiLang

import W7W.Utils

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

--
--
-- fields
--
--

fieldCanonicalName = field "canonical_name" $ return . itemCanonicalName

fieldLang = field "lang" $ return . itemLang
