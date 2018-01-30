{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module W7W.Pictures.Context where

import Data.Monoid ((<>))
import qualified Data.Text as T

import Hakyll

import W7W.Utils
import W7W.Context
import W7W.Pictures.Utils
import W7W.ExifInfo.Types
import W7W.ExifInfo
import W7W.MultiLang (localize, itemLocale, isLocalized, Localized, IsLocalized)

fieldHasPictures :: (Item a -> Pattern) -> Context a
fieldHasPictures pPattern =
  boolFieldM "has_pictures" (hasPictures  pPattern)

fieldPictures :: (Item a -> Pattern) -> Context a
fieldPictures pPattern = listFieldWith "pictures" mkPictureItem (loadPictures' pPattern)
  where
    loadPictures' p i = do
      ps <- loadPictures (p i)
      exifInfos <- mapM (getExifInfo) ps
      mapM (\(pic, ei) -> makeItem (i, pic, ei)) $ zip ps exifInfos

    pictureUrl (Item _ (i, pic, ei)) = fmap (maybe "" toUrl) . getRoute . itemIdentifier $ pic

    exifField :: (Localized a T.Text) => (ExifInfo -> a) -> T.Text -> Item b -> ExifInfo -> Compiler String
    exifField f missingText i ei = return . T.unpack . maybe missingText id . localize (itemLocale i) . f $ ei

    hasExifField :: (IsLocalized a) => (ExifInfo -> a) -> Item b -> ExifInfo -> Compiler Bool
    hasExifField f i ei = return . isLocalized (itemLocale i) . f $ ei

    pictureTitle :: Item (Item a, Item b, ExifInfo) -> Compiler String
    pictureTitle (Item _ (i, _, ei)) = exifField eiTitle "No Title" i ei

    hasPictureTitle :: Item (Item a, Item b, ExifInfo) -> Compiler Bool
    hasPictureTitle (Item _ (i, _, ei)) = hasExifField eiTitle i ei

    pictureDesciption :: Item (Item a, Item b, ExifInfo) -> Compiler String
    pictureDesciption (Item _ (i, _, ei)) = exifField eiDescription "No Description" i ei
    hasPictureDescription (Item _ (i, _, ei)) = hasExifField eiDescription i ei

    pictureCreator (Item _ (i, _, ei)) = exifField eiCreator "No Creator" i ei

    hasPictureCreator (Item _ (i, _, ei)) = hasExifField eiCreator i ei

    hasExifInfo i = fmap (any id) . sequence $ [hasPictureTitle i, hasPictureCreator i, hasPictureDescription i]

    mkPictureItem =
      field "picture_url" pictureUrl
      <> field "picture_title" pictureTitle
      <> boolFieldM "has_picture_title" hasPictureTitle
      <> field "picture_description" pictureDesciption
      <> boolFieldM "has_picture_description" hasPictureDescription
      <> field "picture_creator" pictureCreator
      <> boolFieldM "has_picture_creator" hasPictureCreator
      <> boolFieldM "has_exif_info" hasExifInfo


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
