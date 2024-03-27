{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module W7W.Labels.Parser where

import Data.Yaml

import W7W.Labels.Types

parseLabelsYaml :: IO Labels
parseLabelsYaml = do
  eLabels <- decodeFileEither "labels.yaml"
  either e' return eLabels

  where
    e' s = error $ show s
