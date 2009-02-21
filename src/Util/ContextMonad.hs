module Util.ContextMonad where

import Control.Monad.Error
import Control.Monad


class Monad m => ContextMonad c m | m -> c where
    -- | Given a diagnostic and a computation to take place inside the monad,
    --   run the computation but during it have the diagnostic at the top of the
    --   stack
    withContext :: c -> m a -> m a


instance Error [String] where
    noMsg = []
    strMsg s = [s]


instance ContextMonad String (Either [String]) where
    withContext s (Right x) = Right x
    withContext s (Left cs) = Left  (s:cs)


runSimpleContextMonad :: Either [String] a -> a
runSimpleContextMonad (Left ss) = error $ unlines ss
runSimpleContextMonad (Right x) = x
