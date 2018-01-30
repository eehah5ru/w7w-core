{-# LANGUAGE OverloadedStrings #-}

module W7W.ExifInfo.Parser where

import Prelude hiding (takeWhile, unlines)

import Data.Char (isDigit)
import Data.Text hiding (takeWhile, count)
import Data.Attoparsec.Text
import Control.Applicative

import W7W.ExifInfo.Types

parseRuPart :: Parser Text
parseRuPart = return . pack =<< manyTill anyChar (string " | ")

parseEnPart :: Parser Text
parseEnPart = takeTill $ inClass "\r\n"

fieldTitle :: Text -> Parser Text
fieldTitle t = do
  string t
  skipSpace
  string ": "

emptySpace :: Parser ()
emptySpace = do
  skipSpace
  skipWhile endOfLine
  skipSpace
  where endOfLine = inClass "\r\n"

parseTitle :: Parser Title
parseTitle = do
  fieldTitle "Title"
  ruTitle <- parseRuPart
  enTitle <- parseEnPart
  return $ Title (Just ruTitle) (Just enTitle)


parseCreator :: Parser Creator
parseCreator = do
  fieldTitle "Creator"
  ruCreator <- parseRuPart
  enCreator <- parseEnPart
  return $ Creator (Just ruCreator) (Just enCreator)

parseDescription :: Parser Description
parseDescription = do
  fieldTitle "Description"
  ruDescription <- parseRuPart
  enDescription <- parseEnPart
  return $ Description (Just ruDescription) (Just enDescription)


parseExifInfo :: Parser ExifInfo
parseExifInfo = do
  try emptySpace
  creator <- parseCreator <|> return NoCreator
  emptySpace
  description <- parseDescription <|> return NoDescription
  emptySpace
  title <- parseTitle <|> return NoTitle
  return $ ExifInfo title creator description

--
-- tests
--
testExifInfoData = "Creator                         : Фото Алеся Житкевич, | Photo by Alesia Zhitkevich\n\
\ Description                     : Предоставлено автором фото | Courtesy of the author.\n\
\ Title                           : Документация пира во славу тунеядства Ульяны Быченковой | Documentation of Uliana Bychenkova's Feast for glory of parasitism"

testTitleData = "Title                           : Документация пира во славу тунеядства Ульяны Быченковой | Documentation of Uliana Bychenkova's Feast for glory of parasitism"

testCreatorData = "Creator                         : Фото Алеся Житкевич, | Photo by Alesia Zhitkevich"

testDescriptionData = "Description                     : Предоставлено автором фото | Courtesy of the author."

testEmptyData = ""

testParseTitle :: IO ()
testParseTitle = do
  print $ parseOnly parseTitle testTitleData


testParseExifInfo :: IO ()
testParseExifInfo = do
  print "whole data:"
  print $ testF $ unlines [testCreatorData, testDescriptionData, testTitleData]
  print "without description"
  print $ testF $ unlines [testCreatorData, testTitleData]
  print "empty data"
  print $ testF testEmptyData
  where
    testF d = either id (const "ok") $ parseOnly parseExifInfo d
