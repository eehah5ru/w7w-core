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


cssAndSassRules =
  do
    match "css/_*.scss" $
      compile getResourceBody

    scssDeps <- makePatternDependency "css/_*.scss"
    rulesExtraDependencies [scssDeps] $
      match "css/app.scss" $ do
        route $ setExtension "css"
        compile $ sassCompilerWith sassOptions >>= return . fmap compressCss
    match "css/**/*.css" $ do
      route idRoute
      compile compressCssCompiler
