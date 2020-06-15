{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module W7W.ManyPages.Context where

import Data.Binary
import Data.Typeable

import Control.Monad.Trans.Class
import Control.Monad.Reader

import Hakyll

import W7W.MultiLang
import qualified W7W.Cache as Cache

import Site.Context

import W7W.Context (sortByOrder)

import W7W.ManyPages
import qualified W7W.ManyPages.Config as MPC

pagesPattern :: (MonadReader MPC.Config m) => Locale -> m Pattern
pagesPattern l = asks (MPC.unIndexPagePath . MPC.indexPagePath) >>= return . fromGlob . localizePath l

loadPages :: (Binary a, Typeable a) => Locale ->  ManyPages Compiler [Item a]
loadPages l = do
  pagesPattern l >>= lift . flip loadAllSnapshots "content"

mkPagesField :: ManyPages Compiler (Context String)
mkPagesField = do
  ctx <- mkPageCtx
  pagesFieldName <- asks (MPC.unCtxPagesFieldName . MPC.ctxPagesFieldName)
  cfg <- ask
  return $ listFieldWith pagesFieldName ctx (f cfg)
  where
    f :: MPC.Config -> Item String -> Compiler [Item String]
    f cfg i =
      execManyPages cfg $ loadPages (itemLocale i) >>= lift . sortByOrder

--
-- wrapped site ctx
--
siteCtx :: ManyPages Compiler (Context String)
siteCtx = do
  c <- asks MPC.cache
  lift $ mkSiteCtx c

--
-- page ctx
--
mkPageCtx :: ManyPages Compiler (Context String)
mkPageCtx = do
  c <- asks MPC.cache
  lift $ mkSiteCtx c

--
-- index page ctx
--
mkIndexPageCtx :: ManyPages Compiler (Context String)
mkIndexPageCtx = do
  ctx <- siteCtx
  letters <- mkPagesField
  return $ letters <> ctx
