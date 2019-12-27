{-# LANGUAGE OverloadedStrings #-}
module W7W.Rules.StaticPages where

import Hakyll
import Hakyll.Web.Pandoc

import Data.Monoid ((<>))
import Text.Pandoc.Options
import Text.Pandoc.Extensions
import W7W.MultiLang
import W7W.Compilers.Slim
import W7W.Utils
import W7W.Typography



staticSlimPageRulesM :: Identifier -- rootTpl
                     -> Maybe Identifier -- rootPageTpl
                     -> Maybe Identifier -- pageTpl
                     -> Compiler (Context String) -- context
                     -> FilePath -- path to page without lang prefix
                     -> Rules ()
staticSlimPageRulesM rootTpl mRootPageTpl mPageTpl ctxM path = do
  matchMultiLang rules' rules' path
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
                    -> Rules ()

staticSlimPageRules rootTpl mRootPageTpl mPageTpl ctx path = do
  staticSlimPageRulesM rootTpl mRootPageTpl mPageTpl (return ctx) path

--
-- pandoc compilible static page
--
staticPandocPageRulesM :: Identifier -- root template
                       -> Maybe Identifier -- root page template
                       -> Maybe Identifier -- page specific template
                       -> Compiler (Context String) -- context
                       -> FilePath -- path to page
                       -> Rules ()
staticPandocPageRulesM rootTpl mRootPageTpl mPageTpl ctxM path = do
  matchMultiLang rules' rules' path
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

staticPandocPageRules rootTpl mRootPageTpl mPageTpl ctx path = 
  staticPandocPageRulesM rootTpl mRootPageTpl mPageTpl (return ctx) path


--
-- static html page
--
staticHtmlPageRulesM :: Identifier -- root template
                     -> Maybe Identifier -- root page template
                     -> Maybe Identifier -- page scpecific template
                     -> Compiler (Context String) -- context
                     -> FilePath -- path to page
                     -> Rules ()
staticHtmlPageRulesM rootTpl mRootPageTpl mPageTpl ctxM path = do
  matchMultiLang rules' rules' path
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

staticHtmlPageRules rootTpl mRootPageTpl mPageTpl ctx path =
  staticHtmlPageRulesM rootTpl mRootPageTpl mPageTpl (return ctx) path


customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith customReaderOptions defaultHakyllWriterOptions
    where customReaderOptions = def { readerExtensions = extraReaderExts <> customReaderExts }
          extraReaderExts = extensionsFromList
                              [Ext_auto_identifiers
                              ,Ext_ascii_identifiers
                              ,Ext_emoji
                              ,Ext_backtick_code_blocks
                              ,Ext_footnotes
                              ,Ext_fenced_divs
                              ,Ext_bracketed_spans
                              ,Ext_link_attributes
                              ,Ext_native_divs
                              ,Ext_native_spans
                              ,Ext_raw_html
                              ,Ext_smart
                              ,Ext_implicit_figures]
          customReaderExts = disableExtension Ext_implicit_figures $ pandocExtensions
