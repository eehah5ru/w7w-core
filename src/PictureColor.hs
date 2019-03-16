{-# LANGUAGE OverloadedStrings #-}

module W7W.PictureColor
  (fieldPictureColor)
where

import Data.ByteString.Lazy (ByteString)

import Data.Monoid ((<>))
import Control.Monad ((>=>))
import Control.Applicative ((<|>))

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as T
import qualified Data.Colour.SRGB as SRGB


import Hakyll

import qualified W7W.Cache as Cache

import W7W.PictureColor.Types
import W7W.PictureColor.Parser

fieldPictureColor :: Cache.Caches 
                  -> String -- field name
                  -> (Item a -> Compiler (Maybe Identifier)) -- picture pattern function
                  -> Color -- missing color
                  -> (Color -> Color) -- color change function
                  -> Context a
fieldPictureColor cache fName picturePattern missingColor colorChange =
  field fName (getPictureColor >=> return . colorChange >=> return . formatColor)
  where
    formatColor (SRGB.RGB r g b) = T.unpack $ (T.pack . show) r
                                   <> T.pack ","
                                   <> (T.pack . show) g
                                   <> T.pack ","
                                   <> (T.pack . show) b
                                   
    getCachedPictureColor pictureItem = do
      Cache.compilerLookup (Cache.pictureColorCache cache) 
                           pictureItem
                           
    cachePictureColor pictureItem color = do
      Cache.compilerInsert (Cache.pictureColorCache cache)
                           pictureItem
                           color


    getPictureColor' pictureItem = do
      pH <- pictureHistogram pictureItem
      case pH of
        Histogram (c:cs) -> return $ c
        EmptyHistogram -> return missingColor
        
    getPictureColor i = do
      mPItem <- picturePattern i
      case mPItem of
        Just pItem -> (getCachedPictureColor pItem) <|> (getPictureColor' pItem >>= cachePictureColor pItem)
        Nothing -> return $ missingColor


pictureHistogram :: Identifier -> Compiler Histogram
pictureHistogram i = do
  rawHistogram <- return . T.pack =<< unixFilter "w7w/scripts/picture-histogram.sh" [(toFilePath ) i] ""
  return . either e' id . parseOnly parseHistogram $ rawHistogram
  where
    e' s = error $ unwords ["pictureHistogram: error getting histogram data from ", ((toFilePath) i), "error: ", s ]
