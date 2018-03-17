{-# LANGUAGE OverloadedStrings #-}

module W7W.PictureColor.Operations
  ( opposite
  , saturate)
where

import qualified Data.Colour as C
import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.RGBSpace.HSV as HSV

import W7W.PictureColor.Types

-- rgb :: Color -> SRGB.RGB Int
-- rgb ( r g b) = SRGB.RGB r g b

opposite :: Color -> Color
opposite c = let (h, s, v) = HSV.hsvView c
             in HSV.hsv (oppositeHue h) s v
  where
    oppositeHue h
      | h >= 270 = h + 90 - 360
      | otherwise = h + 90

saturate :: Color -> Color
saturate c = let (h, s, v) = HSV.hsvView c
             in HSV.hsv h s 255
