{-# LANGUAGE FlexibleContexts #-}
module W7W.Labels.Context where

import Hakyll

import Control.Monad.Error.Class
import Control.Monad.Reader
import W7W.MonadCompiler
import qualified W7W.MultiLang as ML
import qualified Data.Text as T

import W7W.Labels.Types

import W7W.Utils

getLabel :: (MonadError [String] m) => Labels -> String -> m Label
getLabel labels path' = label labels path
  where
    path = T.split (=='.') . T.pack $ path'

mkLabelsField :: (MonadReader r m, HasLabels r) => m (Context String)
mkLabelsField = do
  labels <- asks getLabels
  return $ functionField "getLabel" (f labels)

  where
    f labels [] _ = throwError $ ["no args"]
    f labels (path:[]) i = maybe (throwError ["unable to localize" ++ (itemLang i)]) (return . T.unpack) . ML.localizeMaybe (ML.itemLocale i) =<< getLabel labels path
