{-# LANGUAGE OverloadedStrings #-}

module W7W.ManyPages.Rules where

import Control.Monad.Trans.Class
import Control.Monad.Reader

import Hakyll

import W7W.MultiLang
import W7W.Compilers.Markdown

import W7W.ManyPages.Config
import W7W.ManyPages
import W7W.ManyPages.Context
import qualified W7W.ManyPages.Config as MPC


indexPageRules :: Config -> Rules ()
indexPageRules cfg = execManyPages cfg $ do
  indexPagePath <- asks (unIndexPagePath . MPC.indexPagePath)
  cfg <- ask

  lift $ matchMultiLang (rules' cfg) (rules' cfg) (indexPagePath) Nothing

  where
    rules' :: MPC.Config -> Locale -> Rules ()
    rules' cfg locale = do
      markdownPageRules $ \x -> execManyPages cfg $ do
          ctx <- mkIndexPageCtx
          renderer <- asks MPC.rendererIndexPage
          lift $ renderer ctx x

pageRules :: Config -> Rules ()
pageRules cfg  = execManyPages cfg $ do
  pagesPattern <- asks (unPagesPattern . MPC.pagesPattern)
  cfg <- ask

  lift $ matchMultiLang (rules' cfg) (rules' cfg) pagesPattern Nothing

  where
    rules' cfg locale =
      markdownPageRules $ \x -> execManyPages cfg $ do
        ctx <- mkPageCtx
        renderer <- asks MPC.rendererOnePage
        lift $ renderer ctx x
