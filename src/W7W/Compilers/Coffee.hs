{-# LANGUAGE OverloadedStrings #-}
module W7W.Compilers.Coffee where

import           Control.Monad ((>=>))

import Hakyll

coffeeCompiler :: Compiler (Item String)
coffeeCompiler = getResourceString >>= withItemBody processCoffee
  where
    processCoffee = unixFilter "coffee" ["-c", "-s"] >=>
                    unixFilter "yuicompressor" ["--type", "js"]
