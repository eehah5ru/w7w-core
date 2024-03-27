{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module W7W.Pictures.Rules where

import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T

import           System.FilePath
import           System.FilePath.Posix         (takeBaseName, (</>))

import           Hakyll
import           Hakyll.Core.Compiler.Internal (compilerAsk, compilerConfig,
                                                compilerThrow)

import           W7W.Context
import           W7W.Pictures.Utils
import           W7W.Utils
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


data PictureTypeStrategy =
  PicCopyStrategy
  | PicResizeStrategy (Int, Int) deriving (Show)

data PicturesRulesConfig =
  PicturesRulesConfig { pngStrategy    :: PictureTypeStrategy
                      , gifStrategy    :: PictureTypeStrategy
                      , videoStrategy  :: PictureTypeStrategy
                      , othersStrategy :: PictureTypeStrategy
                      } deriving (Show)

copyAllPicturesRulesConfig =
  PicturesRulesConfig { pngStrategy = PicCopyStrategy
                      , gifStrategy = PicCopyStrategy
                      , videoStrategy = PicCopyStrategy
                      , othersStrategy = PicCopyStrategy }

resizeAllPicturesRulesConfig :: (Int, Int) -> PicturesRulesConfig
resizeAllPicturesRulesConfig size =
  PicturesRulesConfig { pngStrategy = PicResizeStrategy size
                      , gifStrategy = PicResizeStrategy size
                      , videoStrategy = PicCopyStrategy
                      , othersStrategy = PicResizeStrategy size }


picturesRules :: PicturesRulesConfig -> FilePath -> Rules ()
picturesRules config basePath = do
  picturesRules' (pngStrategy config) pngPattern
  picturesRules' (gifStrategy config) gifPattern
  picturesRules' (videoStrategy  config) videoPattern
  picturesRules' (othersStrategy config) othersPattern
  where
    mkPattern mExt = fromGlob $ basePath </> ("**/*" ++ (maybe "" id mExt))
    pngPattern = (mkPattern (Just ".png")) .||. (mkPattern (Just ".PNG"))
    gifPattern = (mkPattern (Just ".gif")) .||. (mkPattern (Just ".GIF"))
    videoPattern = (mkPattern (Just ".mp4"))
                   .||. (mkPattern (Just ".MP4"))
                   .||. (mkPattern (Just ".m4v"))
                   .||. (mkPattern (Just ".M4V"))
    othersPattern = (fromGlob $ basePath </> "**/*") .&&. (complement (pngPattern .||. gifPattern .||. videoPattern))

    picturesRules' :: PictureTypeStrategy -> Pattern -> Rules ()
    picturesRules' PicCopyStrategy          = copyPicturesRules'
    picturesRules' (PicResizeStrategy size) = resizePicturesRules' size



copyPicturesRules' :: Pattern -> Rules ()
copyPicturesRules' p = do
  match p $ do
    route idRoute
    compile copyFileCompiler

resizePicturesRules' :: (Int, Int) -> Pattern -> Rules ()
resizePicturesRules' (w, h) p = do
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
