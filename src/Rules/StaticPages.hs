{-# LANGUAGE OverloadedStrings #-}
module W7W.Rules.StaticPages where

import Hakyll
import Hakyll.Web.Pandoc

import Data.Monoid ((<>))
import W7W.MultiLang
import W7W.Compilers.Slim
import W7W.Compilers.Markdown
import W7W.Utils
import W7W.Typography



staticSlimPageRulesM :: Identifier -- rootTpl
                     -> Maybe Identifier -- rootPageTpl
                     -> Maybe Identifier -- pageTpl
                     -> Compiler (Context String) -- context
                     -> Maybe [FilePath] -- excludes without lang prefix
                     -> FilePath -- path to page without lang prefix                  
                     -> Rules ()
staticSlimPageRulesM rootTpl mRootPageTpl mPageTpl ctxM excludes path = do
  matchMultiLang rules' rules' excludes path
  where
    rules' locale =
      slimPageRules $ compilers
      where
        compilers x = do
          ctx <- ctxM
          applyAsTemplate ctx x
            >>= applyCustomPageTemplateSnapshot ctx
            >>= saveSnapshot "content"
            >>= applyMaybeTemplateSnapshot mPageTpl ctx
            >>= applyMaybeTemplateSnapshot mRootPageTpl ctx
            >>= applyTemplateSnapshot rootTpl ctx
            -- >>= relativizeUrls
          
staticSlimPageRules :: Identifier -- rootTpl
                    -> Maybe Identifier -- rootPageTpl
                    -> Maybe Identifier -- pageTpl
                    -> Context String -- context
                    -> Maybe [FilePath] -- excludes without path prefix
                    -> FilePath -- path to page without lang prefix                  
                    -> Rules ()

staticSlimPageRules rootTpl mRootPageTpl mPageTpl ctx excludes path = do
  staticSlimPageRulesM rootTpl mRootPageTpl mPageTpl (return ctx)  excludes path

--
-- pandoc compilible static page
--
staticPandocPageRulesM :: Identifier -- root template
                       -> Maybe Identifier -- root page template
                       -> Maybe Identifier -- page specific template
                       -> Compiler (Context String) -- context
                       -> Maybe [FilePath] -- excludes without prefix
                       -> FilePath -- path to page                       
                       -> Rules ()
staticPandocPageRulesM rootTpl mRootPageTpl mPageTpl ctxM excludes path = do
  matchMultiLang rules' rules' excludes path
  where
    rules' locale = do
      route $ setExtension "html"
      compile $ do
        ctx <- ctxM
        customPandocCompiler
          >>= applyAsTemplate ctx
          >>= beautifyTypography
          >>= applyCustomPageTemplateSnapshot ctx
          >>= saveSnapshot "content"
          >>= applyMaybeTemplateSnapshot mPageTpl ctx
          >>= applyMaybeTemplateSnapshot mRootPageTpl ctx
          >>= applyTemplateSnapshot rootTpl ctx

staticPandocPageRules rootTpl mRootPageTpl mPageTpl ctx excludes path = 
  staticPandocPageRulesM rootTpl mRootPageTpl mPageTpl (return ctx) excludes path


--
-- static html page
--
staticHtmlPageRulesM :: Identifier -- root template
                     -> Maybe Identifier -- root page template
                     -> Maybe Identifier -- page scpecific template
                     -> Compiler (Context String) -- context
                     -> Maybe [FilePath] -- excludes without lang prefix
                     -> FilePath -- path to page                     
                     -> Rules ()
staticHtmlPageRulesM rootTpl mRootPageTpl mPageTpl ctxM excludes path = do
  matchMultiLang rules' rules' excludes path
  where
    rules' locale = do
      route $ setExtension "html"
      compile $ do
        ctx <- ctxM
        getResourceBody
          >>= applyAsTemplate ctx
          >>= beautifyTypography        
          >>= applyCustomPageTemplateSnapshot ctx
          >>= saveSnapshot "content"
          >>= applyMaybeTemplateSnapshot mPageTpl ctx
          >>= applyMaybeTemplateSnapshot mRootPageTpl ctx
          >>= applyTemplateSnapshot rootTpl ctx

staticHtmlPageRules rootTpl mRootPageTpl mPageTpl ctx excludes path =
  staticHtmlPageRulesM rootTpl mRootPageTpl mPageTpl (return ctx) excludes path
