{-# LANGUAGE OverloadedStrings #-}

module W7W.Pictures.Context where

import Hakyll

import W7W.Utils
import W7W.Context
import W7W.Pictures.Utils

fieldHasPictures :: (Item a -> Pattern) -> Context a
fieldHasPictures pPattern =
  boolFieldM "has_pictures" (hasPictures  pPattern)

fieldPictures :: (Item a -> Pattern) -> Context a
fieldPictures pPattern = listFieldWith "pictures" mkPictureItem (loadPictures . pPattern)
  where
    mkPictureItem =
      urlField "picture_url"


fieldFunctionPictureUrl :: (Item a -> Pattern) -> Context a
fieldFunctionPictureUrl pPattern = functionField "pictureUrl" f
  where
    f :: [String] -> Item a -> Compiler String
    f [] _ = errorNoArgs
    f [pId] i =  errorUnimplemented
    f args _ = errorManyArgs args

fieldFunctionPictureCaption :: (Item a -> Pattern) -> Context a
fieldFunctionPictureCaption pPattern = functionField "pictureCaption" f
  where
    f :: [String] -> Item a -> Compiler String
    f [] _ = errorNoArgs
    f [pId] i = errorUnimplemented
    f args _ = errorManyArgs args

fieldFunctionPictureCopyright :: (Item a -> Pattern) -> Context a
fieldFunctionPictureCopyright = undefined
--
--
-- function errors
--
--
errorUnimplemented = error "unimplemented"

errorNoArgs = error "picture identifier should be present"

errorManyArgs args = error $ "only picture identifier is needed " ++ (show args)
