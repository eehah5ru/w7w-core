{-# LANGUAGE OverloadedStrings #-}
module W7W.Compilers.Markdown where

import Hakyll

markdownPageRules :: (Item String -> Compiler (Item String)) -> Rules ()
markdownPageRules f = do
  route $ setExtension "html"
  compile $ pandocCompiler >>= f
