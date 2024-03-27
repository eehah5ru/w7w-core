{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


module W7W.Labels.Types where

-- import Data.Aeson
import Data.Yaml
import GHC.Generics
import Control.Monad.Error.Class

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified W7W.MultiLang as ML
-- import W7W.MultiLang (en, ru, Multilang, MultilangValue)


data Label =
  Label { ru :: !T.Text
        , en :: !T.Text} deriving (Show, Generic)

instance ML.Multilang Label where
  type MultilangValue Label = T.Text

  ru = ru
  en = en

type CategoryName = T.Text

type LabelName = T.Text

type BundleName = T.Text

-- type YearName = String

type Bundle = Map.Map LabelName Label

type Bundles = Map.Map BundleName Bundle

type Labels = Map.Map CategoryName Bundles


--
-- YAML/JSON support
--
instance FromJSON Label


--
-- HasLabels class
--
class HasLabels a where
  getLabels :: a -> Labels

instance HasLabels Labels where
  getLabels = id


instance HasLabels (Labels, a) where
  getLabels = fst

instance HasLabels (a, Labels) where
  getLabels = snd

instance HasLabels (Labels, a, b) where
  getLabels (x, _, _) = x

instance HasLabels (a, Labels, b) where
  getLabels (_, x, _) = x

instance HasLabels (a, b, Labels) where
  getLabels (_, _, x) = x

--
-- get label from labels
--
label :: (MonadError [String] m) => Labels -> [T.Text] -> m Label
label labels path@(cn:bn:ln:[]) = maybe e' return mLabel
  where
    mLabel = Map.lookup cn labels >>= Map.lookup bn >>= Map.lookup ln
    path' = T.unpack $ T.intercalate "." path
    e' = throwError $ ["unable to find label: " ++ path']
