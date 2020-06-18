{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module W7W.ManyPages where

import Control.Monad.Reader

import Hakyll

import W7W.MonadCompiler
import W7W.ManyPages.Config
