{-# LANGUAGE OverloadedStrings #-}
module W7W.Compilers.Markdown where

import Hakyll

import Hakyll.Web.Pandoc

import Text.Pandoc.Options
import Text.Pandoc.Extensions

type CompileF = Item String -> Compiler (Item String)

markdownPageRules :: CompileF -> Rules ()
markdownPageRules f = do
  route $ setExtension "html"
  compile $ customPandocCompiler >>= f

markdownPageRules2 :: CompileF -> CompileF -> Rules ()
markdownPageRules2 beforePandocF afterPandocF = do
  route $ setExtension "html"
  compile $
    getResourceBody
    >>= beforePandocF
    >>= customRenderPandoc
    >>= afterPandocF

customReaderOptions = def { readerExtensions = extraReaderExts <> customReaderExts }
  where
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
    customReaderExts = disableExtension Ext_implicit_figures . disableExtension Ext_tex_math_dollars $ pandocExtensions



customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith customReaderOptions defaultHakyllWriterOptions


customRenderPandoc = renderPandocWith customReaderOptions defaultHakyllWriterOptions
