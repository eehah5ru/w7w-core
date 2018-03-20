{-# LANGUAGE OverloadedStrings #-}
module W7W.Rules.Templates where

import Hakyll

import W7W.Compilers.Slim

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
