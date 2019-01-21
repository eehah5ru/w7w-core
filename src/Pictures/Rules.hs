{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module W7W.Pictures.Rules where

import Data.Monoid ((<>))
import qualified Data.Text as T

import System.FilePath
import System.FilePath.Posix ((</>), takeBaseName)

import Hakyll
import Hakyll.Core.Compiler.Internal (compilerThrow, compilerAsk, compilerConfig)

import W7W.Utils
import W7W.Context
import W7W.Pictures.Utils
-- import W7W.ExifInfo.Types
-- import W7W.ExifInfo
-- import W7W.MultiLang (localize, itemLocale, isLocalized, Localized, IsLocalized)


-- old version
-- picturesRules :: (Int, Int) -> Pattern -> Rules ()
-- picturesRules (w, h) p = do
--   match p $ do
--     route (customRoute pictureRoute)
--     let cmd = "convert"
--     let args = [ "-"
--                    , "-resize"
--                    , concat [show w, "x", show h, "^"]
--                    -- , "-gravity"
--                    -- , "Center"
--                    -- , "-crop"
--                    -- , concat [show h, "x", show h, "+0+0"]
--                    -- , "+repage"
--                    , "-quality"
--                    , "79"
--                    , "-" ]
--     compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)
--   where
--     pictureRoute i =
--       let path = toFilePath i
--       in (replaceExtension path) "jpg"


picturesRules :: (Int, Int) -> Pattern -> Rules ()
picturesRules (w, h) p = do
  match p $ do
    route (customRoute pictureRoute)
    compile $ convertItem
  where
    
    convertItem = do
      i <- getUnderlying
      dstBase <- return . destinationDirectory . compilerConfig =<< compilerAsk
      destPath <- getUnderlying 
                  >>= getRoute >>= maybe (compilerThrow $ ["no route for: " ++ (toFilePath i)] ) return >>= return . (</>) dstBase
      srcPath <- getResourceFilePath
      r0 <- unsafeCompiler (makeDirectories destPath)
      r <- (unixFilter "convert"
                      [ srcPath
                      , "-resize"
                      , concat [show w, "x", show h, "^"]
                      , "-quality"
                      , "79"
                      , destPath]
                      "")
      makeItem ()

    pictureRoute i =
      let path = toFilePath i
      in (replaceExtension path) "jpg"
