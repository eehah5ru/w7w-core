module W7W.MonadCompiler where

import Control.Monad.Trans.Class
import Control.Monad.Reader

import Hakyll

class (Monad m) => MonadCompiler m where
  liftCompiler :: Compiler a -> m a

instance MonadCompiler Compiler where
  liftCompiler = id


instance (MonadCompiler m) => MonadCompiler (ReaderT r m) where
  liftCompiler = lift . liftCompiler
