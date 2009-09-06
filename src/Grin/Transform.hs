{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Grin.Transform
    ( Transform
    , Trans(..)
    , runTrans
    , transformExp
    , renameExp
    ) where

import Grin.Types
import Traverse

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Applicative
import qualified Data.Map as Map

newtype Transform a = Transform { unTransform :: State Int a }
    deriving (Monad, MonadState Int)

class Monad m => Trans m where
    newVariable :: m Renamed

instance Trans Transform where
    newVariable = do u <- get
                     put $! u+1
                     return $ Anonymous u

instance Trans m => Trans (ReaderT r m) where
    newVariable = lift newVariable

instance Trans m => Trans (StateT s m) where
    newVariable = lift newVariable

runTrans :: Transform Grin -> Grin -> Grin
runTrans action grin
    = case runState (unTransform action) (grinUnique grin) of
        (grin, newUnique) -> grin{grinUnique = newUnique}

transformExp :: Trans m => (Expression -> m Expression) -> Grin -> m Grin
transformExp fn grin
    = do defs <- mapM (transformFunc fn) (grinFunctions grin)
         return grin{ grinFunctions = defs }

transformFunc :: Trans m => (Expression -> m Expression) -> FuncDef -> m FuncDef
transformFunc fn def
    = do body <- fn (funcDefBody def)
         return def{funcDefBody = body}




type Rename = ReaderT (Map.Map Renamed Renamed) Transform

renameExp :: Map.Map Renamed Renamed -> Expression -> Transform Expression
renameExp m exp = runReaderT (renameExp' exp) m

renameExp' :: Expression -> Rename Expression
renameExp' (e1 :>>= bind :-> e2)
    = bindArgument bind $ \bind' ->
      tmapM renameExp' (e1 :>>= bind' :-> e2)
renameExp' (e1 :>> e2)
    = liftM2 (:>>) (renameExp' e1) (renameExp' e2)
renameExp' (Case scrut alts)
    = do scrut' <- rename scrut
         Case scrut <$> mapM renameAlt alts
renameExp' (Store v)
    = renameValue Store v
renameExp' (Unit v)
    = renameValue Unit v
renameExp' (Application fn args)
    = Application fn <$> mapM rename args

renameAlt (Node tag nt missing args :> branch)
    = bindArguments args $ \args' ->
      (Node tag nt missing args' :>) <$> renameExp' branch
renameAlt (Vector args :> branch)
    = bindArguments args $ \args' ->
      (Vector args' :>) <$> renameExp' branch
renameAlt (Variable v :> branch)
    = bindArgument v $ \v' ->
      (Variable v' :>) <$> renameExp' branch
renameAlt (cond :> branch)
    = (cond :>) <$> renameExp' branch

bindArgument arg fn
    = do arg' <- newVariable
         local (Map.insert arg arg') $ fn arg'

bindArguments [] fn = fn []
bindArguments (x:xs) fn = bindArgument x $ \x' -> bindArguments xs $ \xs' -> fn (x':xs')

rename :: Renamed -> Rename Renamed
rename val = asks $ Map.findWithDefault val val

renameValue fn (Variable v)
    = renameArgs [v] $ \[v'] -> fn (Variable v')
renameValue fn (Node tag nt missing args)
    = renameArgs args $ \args' -> fn (Node tag nt missing args')
renameValue fn (Vector args)
    = renameArgs args $ \args' -> fn (Vector args')
renameValue fn v
    = return $ fn v

renameArgs args fn
    = do m <- ask
         let worker acc []     = return (fn (reverse acc))
             worker acc (x:xs) = case Map.lookup x m of
                                    Nothing  -> worker (x:acc) xs
                                    Just n   -> worker (n:acc) xs
         worker [] args

