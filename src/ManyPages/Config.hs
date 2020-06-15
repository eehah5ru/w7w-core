{-# LANGUAGE OverloadedStrings #-}

module W7W.ManyPages.Config where

import Hakyll

import qualified W7W.Cache as Cache

newtype PagesPattern = PagesPattern {unPagesPattern :: FilePath}

newtype IndexPagePath = IndexPagePath {unIndexPagePath :: FilePath}

newtype CtxPagesFieldName = CtxPagesFieldName {unCtxPagesFieldName :: String}

--
-- renderers type
--
type RendererIndexPage = Context String -> Item String -> Compiler (Item String)

type RendererPagesList a = Context a -> [Item a] -> Compiler String

type RendererOnePage = Context String -> Item String -> Compiler (Item String)

data Config = Config { indexPagePath :: IndexPagePath
                     , pagesPattern :: PagesPattern
                     , ctxPagesFieldName :: CtxPagesFieldName
                     , rendererIndexPage :: RendererIndexPage
                     , rendererPagesList :: RendererPagesList String
                     , rendererOnePage :: RendererOnePage
                     , cache :: Cache.Caches
                     }

class HasCache a where
  getCache :: a -> Cache.Caches

class HasPagesPattern a where
  getPagesPattern :: a -> PagesPattern
