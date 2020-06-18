{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module W7W.HasVersion where

import Hakyll

data Version = TxtVersion
             | DefaultVersion deriving (Eq)

toVersionPattern :: Version -> Pattern
toVersionPattern TxtVersion = hasVersion "txt"
toVersionPattern DefaultVersion = hasNoVersion

class HasVersion a where
  getVersion :: a -> Version

instance HasVersion Version where
  getVersion = id

instance HasVersion (Version, a) where
  getVersion = fst

instance HasVersion (a, b, Version) where
  getVersion (_, _, x) = x

instance HasVersion (b, Version, a) where
  getVersion (_, x, _) = x

instance HasVersion (Version, a, b) where
  getVersion (x, _, _) = x

--
-- add docs from schedule rules context
--
withVersionedDeps :: Version -> [Pattern] -> Rules b -> Rules b
withVersionedDeps version dPatterns rules  = do
  deps <- mapM makePatternDependency $ map ((.&&.) (toVersionPattern version)) dPatterns
  rulesExtraDependencies deps rules

rulesWithVersion :: Version -> Rules () -> Rules ()
rulesWithVersion DefaultVersion rs = rs
rulesWithVersion TxtVersion rs = version "txt" rs
