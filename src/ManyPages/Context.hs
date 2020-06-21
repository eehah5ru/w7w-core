{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module W7W.ManyPages.Context where

import Data.Binary
import Data.Typeable

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Reader

import Hakyll

import W7W.MultiLang
import qualified W7W.Cache as Cache

import W7W.MonadCompiler

import Site.Context

import W7W.Context (sortByOrder)

import W7W.ManyPages
import qualified W7W.ManyPages.Config as MPC
import W7W.ManyPages.Config hiding (pagesPattern)

pagesPattern :: (MonadReader MPC.Config m) => Locale -> m Pattern
pagesPattern l = asks (MPC.unPagesPattern . MPC.pagesPattern) >>= return . fromGlob . localizePath l

--
-- load pages
--
loadPages :: (Binary a, Typeable a) => Locale ->  ManyPages Compiler [Item a]
loadPages l = do
  pagesPattern l >>= lift . flip loadAllSnapshots "content" >>= lift . sortByOrder

--
-- pages field for index page
--
mkPagesField :: ManyPages Compiler (Context String)
mkPagesField = do
  ctx <- mkPageCtx
  pagesFieldName <- asks (MPC.unCtxPagesFieldName . MPC.ctxPagesFieldName)
  cfg <- ask
  renderer <- asks MPC.rendererPagesList
  return $ field pagesFieldName (f cfg renderer ctx)
  where
    f :: Config -> RendererPagesList String -> Context String -> Item String -> Compiler String
    f cfg renderer ctx i =
      execManyPages cfg (loadPages (itemLocale i)) >>= renderer ctx


--
-- page ctx
--
mkPageCtx :: ManyPages Compiler (Context String)
mkPageCtx = do
  c <- asks MPC.cache
  fields' <- asks MPC.pageCtxFields
  liftA2 (<>) siteCtx fields'

--
-- index page ctx
--
mkIndexPageCtx :: ManyPages Compiler (Context String)
mkIndexPageCtx = do
  ctx <- siteCtx
  pages <- mkPagesField
  return $ pages <> ctx
