{-# LANGUAGE OverloadedStrings #-}

module W7W.Context.Media where

import Hakyll

import W7W.Context
import W7W.Pictures.Context
import W7W.Pictures.Utils

--
--
-- metadata predicates
--
--
hasYoutubeVideoId :: MonadMetadata m => Item a -> m Bool
hasYoutubeVideoId = hasItemField "youtubeVideoId"

hasYoutubeVideoNumId :: MonadMetadata m => Int -> Item a -> m Bool
hasYoutubeVideoNumId n i = hasItemField ("youtubeVideoId0" ++ (show n)) i

hasVimeoVideoId :: MonadMetadata m => Item a -> m Bool
hasVimeoVideoId = hasItemField "vimeoVideoId"

hasVimeoVideoNumId :: MonadMetadata m => Int -> Item a -> m Bool
hasVimeoVideoNumId n i = hasItemField ("vimeoVideoId0" ++ (show n)) i

hasSoundcloudTrackId :: MonadMetadata m => Item a -> m Bool
hasSoundcloudTrackId = hasItemField "soundcloudTrackId"

hasSoundcloudTrackNumId :: MonadMetadata m => Int -> Item a -> m Bool
hasSoundcloudTrackNumId n = hasItemField ("soundcloudTrackId0" ++ (show n))

hasMixcloudTrackId :: MonadMetadata m => Item a -> m Bool
hasMixcloudTrackId = hasItemField "mixcloudTrackId"

hasMixcloudTrackNumId :: MonadMetadata m => Int -> Item a -> m Bool
hasMixcloudTrackNumId n = hasItemField ("mixcloudTrackId0" ++ (show n))


hasVideo :: MonadMetadata m => Item a -> m Bool
hasVideo i =  sequence videoPredicates >>= return . any id
  where
    videoPredicates = [ hasYoutubeVideoId i
                      , hasVimeoVideoId i ] ++
                      (map (\n -> hasYoutubeVideoNumId n i) [1..9]) ++
                      (map (\n -> hasVimeoVideoNumId n i)) [1..9]

hasAudio :: MonadMetadata m => Item a -> m Bool
hasAudio i = sequence audioPredicates >>= return . any id
  where
    audioPredicates = [ hasSoundcloudTrackId i
                      , hasMixcloudTrackId i ] ++
                      (map (\n -> hasMixcloudTrackNumId n i) [1..9]) ++
                      (map (\n -> hasSoundcloudTrackNumId n i) [1..9])

fieldHasVideo = do
  boolFieldM "hasVideo" hasVideo
  -- where
  --   f i = do
  --     x <- hasVideo i
  --     unsafeCompiler $ do
  --       putStrLn $ (show $ itemIdentifier i) ++ " - hasVideo: " ++ (show x)
  --     return x

-- fieldHasVideoText = do
--   field "hasVideoText"
--   where
--     f i = do
--       x <- hasVideo i
--       unsafeCompiler $ do
--         putStrLn $ (show $ itemIdentifier i) ++ " - hasVideoText: " ++ (show x)
--       return (show x)


fieldHasAudio = do
  boolFieldM "hasAudio" hasAudio

-- fieldHasImages =
--   boolFieldM "hasImages" hasImages


fieldHasMedia picturesPattern =
  boolFieldM "hasMedia" hasMedia'
  where
    hasMedia' i = sequence ps >>= return  . any id
     where ps = [hasPictures picturesPattern i
                ,hasVideo i
                ,hasAudio i]
