{-# LANGUAGE OverloadedStrings #-}
module W7W.Rules.Templates where

import Hakyll

import W7W.Compilers.Slim
import W7W.Compilers.Markdown
import W7W.Utils
import W7W.Typography

templatesRules =
  do
    match ("templates/*.html" .||. "templates/**/*.html") $ compile $ templateCompiler >>= saveSnapshot "template"


    match "templates/_*.slim" $
      compile getResourceBody

    slimDeps <- makePatternDependency "templates/_*.slim"
    rulesExtraDependencies [slimDeps] $
      match ("templates/*.slim" .&&. (complement "templates/_*.slim")) $ do
        compile $
          getResourceString
            >>= withItemBody compileSlimWithEmptyLocals
            >>= withItemBody (return . readTemplate)
            >>= saveSnapshot "template"

    match ("templates/*.md") $ do
      compile $
        customPandocCompiler
          >>= withItemBody (return . readTemplate)
          >>= saveSnapshot "template"
