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

staticSlimPageRules :: Identifier -- rootTpl
                    -> Maybe Identifier -- rootPageTpl
                    -> Maybe Identifier -- pageTpl
                    -> Context String -- context
                    -> FilePath -- path to page without lang prefix
                    -> Rules ()

staticSlimPageRules rootTpl mRootPageTpl mPageTpl ctx path = do
  matchMultiLang rules' rules' path
  where
    rules' locale =
      slimPageRules $ compilers
      where
        compilers x =
          applyAsTemplate ctx x
          >>= applyCustomPageTemplateSnapshot ctx
          >>= applyMaybeTemplateSnapshot mPageTpl ctx
          >>= applyMaybeTemplateSnapshot mRootPageTpl ctx
          >>= applyTemplateSnapshot rootTpl ctx
          -- >>= relativizeUrls

--
-- pandoc compilible static page
--
staticPandocPageRules :: Identifier -- root template
                      -> Maybe Identifier -- root page template
                      -> Maybe Identifier -- page specific template
                      -> Context String -- context
                      -> FilePath -- path to page
                      -> Rules ()
staticPandocPageRules rootTpl mRootPageTpl mPageTpl ctx path = do
  matchMultiLang rules' rules' path
  where
    rules' locale = do
      route $ setExtension "html"
      compile $ customPandocCompiler
        >>= beautifyTypography
        >>= applyCustomPageTemplateSnapshot ctx
        >>= applyMaybeTemplateSnapshot mPageTpl ctx
        >>= applyMaybeTemplateSnapshot mRootPageTpl ctx
        >>= applyTemplateSnapshot rootTpl ctx

--
-- statci html page
--
staticHtmlPageRules :: Identifier -- root template
                    -> Maybe Identifier -- root page template
                    -> Maybe Identifier -- page scpecific template
                    -> Context String -- context
                    -> FilePath -- path to page
                    -> Rules ()
staticHtmlPageRules rootTpl mRootPageTpl mPageTpl ctx path = do
  matchMultiLang rules' rules' path
  where
    rules' locale = do
      route $ setExtension "html"
      compile $ getResourceBody
        >>= applyCustomPageTemplateSnapshot ctx
        >>= applyMaybeTemplateSnapshot mPageTpl ctx
        >>= applyMaybeTemplateSnapshot mRootPageTpl ctx
        >>= applyTemplateSnapshot rootTpl ctx


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
                              ,Ext_smart]
          customReaderExts = disableExtension Ext_implicit_figures $ pandocExtensions
