{-# LANGUAGE OverloadedStrings #-}

module W7W.PictureColor.Types
  ( Color
  , Histogram (..)
  , SRGB.RGB (..)
  , mkColor)
where

import qualified Data.Colour as C
import qualified Data.Colour.SRGB as SRGB

type Color = SRGB.RGB Double

data Histogram  = Histogram [Color]
               | EmptyHistogram

mkColor r g b = SRGB.RGB r g b
