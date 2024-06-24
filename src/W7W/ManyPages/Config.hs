{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module W7W.ManyPages.Config where

import Hakyll

import Control.Monad.Trans.Class
import Control.Monad.Reader

import W7W.MonadCompiler

import W7W.HasVersion
import W7W.Utils

import W7W.Labels.Types

import qualified W7W.Cache as Cache
import W7W.Context (SiteCtxFactory)

newtype PagesPattern = PagesPattern {unPagesPattern :: FilePath}

newtype IndexPagePath = IndexPagePath {unIndexPagePath :: FilePath}

newtype CtxPagesFieldName = CtxPagesFieldName {unCtxPagesFieldName :: String}

--
-- renderers type
--
type RendererIndexPage = Context String -> Item String -> Compiler (Item String)

type RendererPagesList a = Context a -> [Item a] -> Compiler String

type RendererOnePage = Context String -> Item String -> Compiler (Item String)

--
-- ctx fields
--
-- type CtxFields r m = (MonadReader r m, MonadCompiler m, Cache.HasCache r, HasVersion r) => m (Context String)

type CtxFields = ManyPages Compiler (Context String)

data Config = Config { indexPagePath :: IndexPagePath
                     , pagesPattern :: PagesPattern
                     , ctxPagesFieldName :: CtxPagesFieldName
                     , rendererIndexPage :: RendererIndexPage
                     , rendererPagesList :: RendererPagesList String
                     , rendererOnePage :: RendererOnePage
                     , pageCtxFields :: CtxFields
                     , cache :: Cache.Caches
                     , labels :: Labels
                     , siteCtxFactory :: SiteCtxFactory
                     }

class HasPagesPattern a where
  getPagesPattern :: a -> PagesPattern

instance Cache.HasCache Config where
  getCache = cache

instance HasLabels Config where
  getLabels = labels

--
--
-- execution context
--
--
newtype ManyPages m a = ManyPages {unwrapManyPages :: ReaderT Config m a} deriving (Functor, Applicative, Monad, MonadReader Config, MonadTrans)

execManyPages c = flip runReaderT c . unwrapManyPages

instance MonadCompiler (ManyPages Compiler) where
  liftCompiler = lift . liftCompiler
