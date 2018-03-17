{-# LANGUAGE OverloadedStrings #-}

module W7W.PictureColor.Parser
  (parseHistogram)
  where

import Prelude hiding (takeWhile, unlines)

import Data.Char (isDigit)
import Data.Text hiding (takeWhile, count)
import Data.Attoparsec.Text
import Control.Applicative

import W7W.PictureColor.Types

emptySpace :: Parser ()
emptySpace = do
  skipSpace
  skipWhile endOfLine
  skipSpace
  where endOfLine = inClass "\r\n"

parseColor :: Parser Color
parseColor = do
  string "("
  skipSpace
  r <- parseDouble
  string ","
  skipSpace
  g <- parseDouble
  string ","
  skipSpace
  b <- parseDouble
  string ")"
  return $ mkColor r g b
  where
    parseDouble = return . fromIntegral =<<  decimal


parseHistogram :: Parser Histogram
parseHistogram = do
  histogram' <|> emptyHistogram
  where
    emptyHistogram = return EmptyHistogram
    parseLevel = decimal
    histogram' = do
      colors <- count 5 histogramLine
      return $ Histogram colors
    histogramLine = do
        try emptySpace
        level <- parseLevel
        string ": "
        color <- parseColor
        skipWhile (not . isEndOfLine) >> endOfLine
        return color


--
--
-- tests
--
--

testEmptyData = ""

testColorData = "( 90, 93, 88)"

testHistogramData = "    179624: ( 90, 93, 88) #5A5D58 srgb(90,93,88)\
\    172756: ( 47, 54, 55) #2F3637 srgb(47,54,55)\
\    139610: (153,166,157) #99A69D srgb(153,166,157)\
\    134986: (207,205,197) #CFCDC5 srgb(207,205,197)\
\     64224: (132,124,120) #847C78 srgb(132,124,120)"


testParseColor :: IO ()
testParseColor = do
  print $ testF testColorData
  where
    testF d = either id (const "ok") $ parseOnly parseColor d

testParseHistogram :: IO ()
testParseHistogram = do
  print "whole data:"
  print $ testF testHistogramData
  print "empty data:"
  print $ testF testEmptyData
  where
    testF d = either id (const "ok") $ parseOnly parseHistogram d
