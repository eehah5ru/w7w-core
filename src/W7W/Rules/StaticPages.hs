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
                     -> FilePath -- path to page without lang prefix
                     -> Maybe [FilePath] -- excludes without lang prefix
                     -> Rules ()
staticSlimPageRulesM rootTpl mRootPageTpl mPageTpl ctxM path excludes = do
  matchMultiLang rules' rules' path excludes
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
                    -> FilePath -- path to page without lang prefix
                    -> Maybe [FilePath] -- excludes without path prefix
                    -> Rules ()

staticSlimPageRules rootTpl mRootPageTpl mPageTpl ctx path excludes = do
  staticSlimPageRulesM rootTpl mRootPageTpl mPageTpl (return ctx) path excludes

--
-- pandoc compilible static page
--
staticPandocPageRulesM :: Identifier -- root template
                       -> Maybe Identifier -- root page template
                       -> Maybe Identifier -- page specific template
                       -> Compiler (Context String) -- context
                       -> FilePath -- path to page
                       -> Maybe [FilePath] -- excludes without prefix
                       -> Rules ()
staticPandocPageRulesM rootTpl mRootPageTpl mPageTpl ctxM path excludes = do
  matchMultiLang rules' rules' path excludes
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

staticPandocPageRules rootTpl mRootPageTpl mPageTpl ctx path excludes = 
  staticPandocPageRulesM rootTpl mRootPageTpl mPageTpl (return ctx) path excludes


--
-- static html page
--
staticHtmlPageRulesM :: Identifier -- root template
                     -> Maybe Identifier -- root page template
                     -> Maybe Identifier -- page scpecific template
                     -> Compiler (Context String) -- context
                     -> FilePath -- path to page
                     -> Maybe [FilePath] -- excludes without lang prefix
                     -> Rules ()
staticHtmlPageRulesM rootTpl mRootPageTpl mPageTpl ctxM path excludes = do
  matchMultiLang rules' rules' path excludes
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

staticHtmlPageRules rootTpl mRootPageTpl mPageTpl ctx path excludes =
  staticHtmlPageRulesM rootTpl mRootPageTpl mPageTpl (return ctx) path excludes
