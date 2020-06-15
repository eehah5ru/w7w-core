{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module W7W.ManyPages where

import Control.Monad.Reader

import Hakyll

import W7W.ManyPages.Config

newtype ManyPages m a = ManyPages {unwrapManyPages :: ReaderT Config m a} deriving (Functor, Applicative, Monad, MonadReader Config, MonadTrans)

execManyPages c = flip runReaderT c . unwrapManyPages
