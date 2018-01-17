{-# LANGUAGE OverloadedStrings #-}
module W7W.Rules.StaticPages where

import Hakyll

import W7W.MultiLang
import W7W.Compilers.Slim
import W7W.Utils

staticSlimPageRules :: Identifier -- rootTpl
                    -> Identifier -- pageTpl
                    -> Context String -- context
                    -> FilePath -- path to page without lang prefix
                    -> Rules ()

staticSlimPageRules rootTpl pageTpl ctx path = do
  matchMultiLang rules' rules' path
  where
    rules' locale =
      slimPageRules $ compilers
      where compilers x =
              applyAsTemplate ctx x
              >>= applyCustomPageTemplateSnapshot ctx
              >>= applyTemplateSnapshot pageTpl ctx
              >>= applyTemplateSnapshot rootTpl ctx
              -- >>= relativizeUrls

--
-- pandoc compilible static page
--
staticPandocPageRules :: Identifier -- root template
                      -> Identifier -- page template
                      -> Context String -- context
                      -> FilePath -- path to page
                      -> Rules ()
staticPandocPageRules rootTpl pageTpl ctx path = do
  matchMultiLang rules' rules' path
  where
    rules' locale = do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= applyCustomPageTemplateSnapshot ctx
        >>= applyTemplateSnapshot pageTpl ctx
        >>= applyTemplateSnapshot rootTpl ctx

--
-- statci html page
--
staticHtmlPageRules :: Identifier -- root template
                    -> Identifier -- page template
                    -> Context String -- context
                    -> FilePath -- path to page
                    -> Rules ()
staticHtmlPageRules rootTpl pageTpl ctx path = do
  matchMultiLang rules' rules' path
  where
    rules' locale = do
      route $ setExtension "html"
      compile $ getResourceBody
        >>= applyCustomPageTemplateSnapshot ctx
        >>= applyTemplateSnapshot pageTpl ctx
        >>= applyTemplateSnapshot rootTpl ctx
