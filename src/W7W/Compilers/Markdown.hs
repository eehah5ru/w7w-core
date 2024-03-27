{-# LANGUAGE OverloadedStrings #-}
module W7W.Compilers.Markdown where

import Hakyll

import Hakyll.Web.Pandoc
import Text.Pandoc

import W7W.Compilers.W7WPandoc as W7WPandoc
import Text.Pandoc.Options
import Text.Pandoc.Extensions

import qualified Data.Text as T

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
                        ,Ext_attributes
                        ,Ext_native_divs
                        ,Ext_native_spans
                        ,Ext_raw_html
                        ,Ext_smart
                        ,Ext_implicit_figures]
    customReaderExts = disableExtension Ext_implicit_figures . disableExtension Ext_tex_math_dollars $ pandocExtensions



customPandocCompiler :: Compiler (Item String)
customPandocCompiler = W7WPandoc.pandocCompilerWith customReaderOptions defaultHakyllWriterOptions


customRenderPandoc = W7WPandoc.renderPandocWith customReaderOptions defaultHakyllWriterOptions

-- --
-- -- from here
-- -- https://hackage.haskell.org/package/hakyll-4.16.2.0/docs/src/Hakyll.Web.Pandoc.html#readPandoc
-- -- | Render the resource using pandoc
-- renderPandocWith'
--     :: ReaderOptions -> WriterOptions -> Item String -> Compiler (Item String)
-- renderPandocWith' ropt wopt item =
--     writePandocWith wopt <$> readPandocWith' ropt item


-- --
-- -- rewritten to use CommonMark by default
-- --
-- --------------------------------------------------------------------------------
-- -- | Read a string using pandoc, with the supplied options
-- readPandocWith'
--     :: ReaderOptions           -- ^ Parser options
--     -> Item String             -- ^ String to read
--     -> Compiler (Item Pandoc)  -- ^ Resulting document
-- readPandocWith' ropt item =
--     case runPure $ traverse (reader ropt (itemFileType item)) (fmap T.pack item) of
--         Left err    -> fail $
--             "Hakyll.Web.Pandoc.readPandocWith: parse failed: " ++ show err
--         Right item' -> return item'
--   where
--     reader ro t = case t of
--         DocBook            -> readDocBook ro
--         Html               -> readHtml ro
--         Jupyter            -> readIpynb ro
--         LaTeX              -> readLaTeX ro
--         LiterateHaskell t' -> reader (addExt ro Ext_literate_haskell) t'
--         Markdown           -> readCommonMark ro
--         MediaWiki          -> readMediaWiki ro
--         OrgMode            -> readOrg ro
--         Rst                -> readRST ro
--         Textile            -> readTextile ro
--         _                  -> error $
--             "Hakyll.Web.readPandocWith: I don't know how to read a file of " ++
--             "the type " ++ show t ++ " for: " ++ show (itemIdentifier item)

--     addExt ro e = ro {readerExtensions = enableExtension e $ readerExtensions ro}

-- --
-- -- redefined!
-- --
-- -- | A version of 'pandocCompiler' which allows you to specify your own pandoc
-- -- options
-- pandocCompilerWith' :: ReaderOptions -> WriterOptions -> Compiler (Item String)
-- pandocCompilerWith' ropt wopt =
--     cached "Hakyll.Web.Pandoc.pandocCompilerWith'" $
--         pandocCompilerWithTransform ropt wopt id
