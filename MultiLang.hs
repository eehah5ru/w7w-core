{-# LANGUAGE OverloadedStrings #-}

module Site.MultiLang where

import System.FilePath.Posix ((</>))
import Data.Maybe (fromMaybe)

import Hakyll
import Hakyll.Core.Util.String (replaceAll)
import Site.Types

import Site.Utils

data Locale = RU | EN | UNKNOWN deriving (Show)

fromLang :: String -> Locale
fromLang "ru" = RU
fromLang "en" = EN
fromLang l = error $ unwords ["unknown lang: ", l]

toLang :: Locale -> String
toLang RU = "ru"
toLang EN = "en"
toLang l = error $ unwords ["unknown localizer: ", show l]

matchMultiLang :: (Locale -> Rules ())
               -> (Locale -> Rules ())
               -> FilePath
               -> Rules ()
matchMultiLang ruRules enRules path =
  do match ruPages $ ruRules RU
     match enPages $ enRules EN
  where ruPages = fromGlob $ localizePath RU path
        enPages = fromGlob $ localizePath EN path


bothLangsPattern :: String -> Pattern
bothLangsPattern p =
  (fromGlob (localizePath RU p)) .||. (fromGlob (localizePath EN p))

localizeUrl :: Locale -> String -> String
localizeUrl l [] = "/" ++ (toLang l) ++ "/"
localizeUrl l url = "/" ++ (toLang l) ++ "/" ++ url'
  where
    url' = case (head url) of
             '/' -> tail url
             _ -> url

localizePath :: Locale -> String -> String
localizePath l [] = (toLang l) ++ "/"
localizePath l path = (toLang l) </> path

localizeField :: Locale -> String -> String
localizeField l f = f ++ "_" ++ (toLang l)


chooseByItemLang :: String -> String -> Item a -> String
chooseByItemLang r e = chooseByLocale r e . fromLang . itemLang

chooseByLocale :: String -> String -> Locale -> String
chooseByLocale r _ RU = r
chooseByLocale _ e EN = e
chooseByLocale _ _ _ = error "unknown locale"

otherLang :: String -> String
otherLang l = case l of
                "ru" -> "en"
                "en" -> "ru"
                _ -> "unknown"


fieldOtherLang :: Context String
fieldOtherLang =
  field "other_lang" (return . otherLang . itemLang)


fieldOtherLangUrl :: Context String
fieldOtherLangUrl =
  field "other_lang_url" getOtherLangUrl
  where
    getOtherLangUrl i = do
      u <- return . fromMaybe "/not-found.html" =<< getRoute =<< (return . itemIdentifier) i
      return . toUrl $ (replaceAll (pattern' i) (replacementF i) u)
    pattern' i = (itemLang i) ++ "/"
    replacementF i = const $ (otherLang . itemLang $ i) ++ "/"


fieldRuUrl :: Context String
fieldRuUrl = multiLangUrlField "ru" "en"

fieldEnUrl :: Context String
fieldEnUrl = multiLangUrlField "en" "ru"

multiLangUrlField :: String -> String -> Context String
multiLangUrlField lang fromLang = field fieldName (\x -> getUrl fromLang x >>= return . translateUrl fromLang lang >>= return . (++) "/")
  where notFoundPage :: String -> String
        notFoundPage lang = (lang ++ "/2017/404.html")

        fieldName = lang ++ "_url"

        getUrl :: String -> Item a -> Compiler String
        getUrl langPrefix i = return (itemIdentifier i)
          -- >>= (load :: Identifier -> Compiler (Item String))
          -- >>= return . itemIdentifier
          >>= getRoute
          >>= return . maybe (notFoundPage langPrefix) id

        translateUrl :: String -> String -> String -> String
        translateUrl fromLang toLang = replaceAll (fromLang ++ "/") (const (toLang ++ "/"))
        fallbackToNotFound :: String -> String -> Compiler String
        fallbackToNotFound lang u =
          do debugCompiler ("MultiLang-before: " ++ u)
             fallbackUrl <- return u >>= return . fromFilePath >>= getRoute >>= return . maybe (notFoundPage lang) id
             debugCompiler ("MultiLang-after: " ++ fallbackUrl)
             return fallbackUrl
