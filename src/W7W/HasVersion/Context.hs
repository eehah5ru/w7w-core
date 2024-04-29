{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module W7W.HasVersion.Context where

import Hakyll

import W7W.HasVersion

fieldIsHtmlVersion :: (HasVersion v) => v -> Context a
fieldIsHtmlVersion v =
  boolField "isHtmlVersion" $ const (isHtml . getVersion $ v)

fieldIsTxtVersion :: (HasVersion v) => v -> Context a
fieldIsTxtVersion v =
  boolField "isTxtVersion" $ const (isTxt . getVersion $ v)
