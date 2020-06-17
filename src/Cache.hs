{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module W7W.Cache
  ( C.Cache (..)
  , Caches (..)
  , HasCache
  , getCache
  , newCaches
  , compilerLookup
  , compilerInsert
  ) where

import           Control.Monad.Trans            (liftIO)
import           Control.Applicative            (Alternative (..)) 

import qualified Data.Cache as C
import Data.Hashable

import Hakyll

import W7W.PictureColor.Types
import W7W.ExifInfo.Types

-- type Cache a = C.Cache String a

data Caches = Caches { pictureColorCache :: C.Cache Identifier Color
                     , exifInfoCache :: C.Cache Identifier ExifInfo
                     , revisionCache :: C.Cache Identifier String}

class HasCache a where
  getCache :: a -> Caches

-- class CacheType a b where
--   getCache :: Caches -> C.Cache a b

-- instance CacheType Identifier Color where
--   getCache = pictureColorCache 

instance Hashable Identifier where
  hashWithSalt s x = hashWithSalt s (toFilePath x)


newCaches :: IO (Caches)
newCaches = Caches <$> C.newCache Nothing <*> C.newCache Nothing <*> C.newCache Nothing

compilerLookup :: (Eq k, Hashable k) => C.Cache k v -> k -> Compiler v
compilerLookup c k = do
  mV <- unsafeCompiler (C.lookup c k)
  maybe empty return mV

compilerInsert :: (Eq k, Hashable k) => C.Cache k v -> k -> v -> Compiler v
compilerInsert c k v = do
  unsafeCompiler $ C.insert c k v
  return v
