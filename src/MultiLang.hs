{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module W7W.MultiLang where

import System.FilePath.Posix ((</>))
import Data.Maybe (fromMaybe)

import Hakyll
import Hakyll.Core.Util.String (replaceAll)
import W7W.Types

import W7W.Utils

data Locale = RU | EN | UNKNOWN deriving (Show)


class Localized a b where
  localize :: Locale -> a -> Maybe b

class IsLocalized a where
  isLocalized :: Locale -> a -> Bool

--
-- Multilang type class
--


class Multilang a where
  type MultilangValue a

  ru :: a -> MultilangValue a
  en :: a -> MultilangValue a

localizeMaybe :: (Multilang a) => Locale -> a -> Maybe (MultilangValue a)
localizeMaybe l x =
  case l of
    RU -> Just . ru $ x
    EN -> Just . en $ x
    _ -> Nothing


-- class Multilang2 v a where
--   -- type MultilangValue a

--   ru2 :: a -> v
--   en2 :: a -> v

-- class Multilang3 a where
--   type MultilangValue3 a

--   ru3 :: a -> MultilangValue3 a
--   en3 :: a -> MultilangValue3 a

fromLang :: String -> Locale
fromLang "ru" = RU
fromLang "en" = EN
fromLang l = error $ unwords ["unknown lang: ", l]

fromLangMaybe :: String -> Maybe Locale
fromLangMaybe "ru" = Just RU
fromLangMaybe "en" = Just EN
fromLangMaybe l = Nothing

itemLocale :: Item a -> Locale
itemLocale = fromLang . itemLang

toLang :: Locale -> String
toLang RU = "ru"
toLang EN = "en"
toLang l = error $ unwords ["unknown localizer: ", show l]

matchMultiLang :: (Locale -> Rules ())
               -> (Locale -> Rules ())
               -> FilePath
               -> Maybe [FilePath] -- exclude
               -> Rules ()
matchMultiLang ruRules enRules path excludes =
  do match (pages RU) $ ruRules RU
     match (pages EN) $ enRules EN
  where
    pattern' l = fromGlob . localizePath l
    excludes' l Nothing = Nothing
    excludes' l (Just []) = Nothing
    excludes' l (Just xs) = Just $ foldl f' (head ps') (tail ps')
      where
        ps' = fmap (pattern' l) xs
        f' r p = r .||. p
    pages l = case excludes' l excludes of
                Nothing -> (pattern' l path)
                Just exs -> (pattern' l path) .&&. (complement exs)


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

-- localizePattern :: Locale -> Pattern -> Pattern
-- localizePattern l p = undefined

localizeField :: Locale -> String -> String
localizeField l f = f ++ "_" ++ (toLang l)


chooseByItemLang :: String -> String -> Item a -> String
chooseByItemLang r e = chooseByLocale r e . fromLang . itemLang

chooseByLocale :: a -> a -> Locale -> a
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
  field "otherLang" (return . otherLang . itemLang)


fieldOtherLangUrl :: Context String
fieldOtherLangUrl =
  field "otherLangUrl" getOtherLangUrl
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

        fieldName = lang ++ "Url"

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
