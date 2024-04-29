{-# LANGUAGE OverloadedStrings #-}
module W7W.Rules.Assets where

import           Hakyll.Web.Sass (sassCompilerWith)

import Hakyll

import W7W.Types
import W7W.Config

dataRules =
  match "data/**" $
        do route idRoute
           compile copyFileCompiler

imagesRules =
  match "images/**" $
  do route idRoute
     compile copyFileCompiler

fontsRules =
  match "fonts/*" $
        do route idRoute
           compile copyFileCompiler

jsRules =
  do match "js/*.js" $
           do route idRoute
              compile copyFileCompiler
     match "js/vendor/*.js" $
           do route idRoute
              compile copyFileCompiler


cssAndSassRules :: Pattern -> [Pattern] -> Rules ()
cssAndSassRules scssDepsPattern  scssFilePatterns =
  do
    match scssDepsPattern $
      compile getResourceBody

    scssDeps <- makePatternDependency scssDepsPattern
    --
    -- app
    --
    mapM_ (scssRulesWithDeps scssDeps) scssFilePatterns

    --
    -- all css files
    --
    match "css/**/*.css" $ do
      route idRoute
      compile compressCssCompiler
  where
    -- -- make file list from file patterns
    -- scssFiles =
    --   do
         
    scssRulesWithDeps scssDeps p =
      rulesExtraDependencies [scssDeps] $
        match p scssRules

    scssRules = do
      route $ setExtension "css"
      compile $ sassCompilerWith sassOptions >>= return . fmap compressCss
