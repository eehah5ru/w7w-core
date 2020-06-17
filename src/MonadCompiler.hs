module W7W.MonadCompiler where

import Hakyll

class (Monad m) => MonadCompiler m where
  liftCompiler :: Compiler a -> m a

instance MonadCompiler Compiler where
  liftCompiler = id
