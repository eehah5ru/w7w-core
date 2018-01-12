{-# LANGUAGE OverloadedStrings #-}
module W7W.Compilers.Slim where

import qualified Data.Aeson as AE
import           Data.String.Conversions (convertString)
import           Hakyll
import W7W.Types



-- loadAndApplySlimTemplate tplPattern ctx x = do
--   tpl <- loadBody tplPattern >>= undefined
--   undefined

slimPageRules :: (Item String -> Compiler (Item String)) -> Rules ()
slimPageRules f =
   withSlimDeps $ do
     route $ setExtension "html"
     compile $ slimCompiler
       >>= f
       -- >>= relativizeUrls


withSlimDeps rules =
  do ruSlimDeps <- makePatternDependency "ru/shared/*.slim"
     enSlimDeps <- makePatternDependency "en/shared/*.slim"
     rulesExtraDependencies [enSlimDeps, ruSlimDeps] rules


slimCompiler :: Compiler (Item String)
slimCompiler =
  do
    m <- getUnderlying >>= getMetadata >>= makeJson
    getResourceBody >>= withItemBody (compileSlim m)
  where makeJson = return . convertString . AE.encode

slimCompilerWithEmptyLocals :: Compiler (Item String)
slimCompilerWithEmptyLocals =
  getResourceBody >>= withItemBody compileSlimWithEmptyLocals


compileSlim :: String -> String -> Compiler String
compileSlim m = unixFilter "slimrb" [ "-s"
                                       , "-p"
                                       , "-r ./slimlib.rb"
                                       ,  "--trace"
                                       , "-l"
                                       , m]

compileSlimWithEmptyLocals :: String -> Compiler String
compileSlimWithEmptyLocals = compileSlim "{}"
