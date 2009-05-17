{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Grin.Optimize.Simple
    ( optimize
    ) where

import Grin.Types

import Control.Monad.Reader
import qualified Data.Map as Map



newtype Opt a = Opt {unOpt :: Reader Subst a}
    deriving (MonadReader Subst, Monad)
type Subst = Map.Map Renamed Renamed


optimize :: Grin -> Grin
optimize grin
    = grin{ grinFunctions = map simpleFuncDef (grinFunctions grin)}


simpleFuncDef :: FuncDef -> FuncDef
simpleFuncDef def
    = def{ funcDefBody = runReader (unOpt (simpleExpression (funcDefBody def))) Map.empty }

simpleExpression :: Expression -> Opt Expression
simpleExpression (Unit (Variable v1) :>>= v2 :-> t)
    = do v1' <- doSubst v1
         subst v2 v1' (simpleExpression t)
simpleExpression ((a :>>= b :-> c) :>>= d)
    = simpleExpression (a :>>= b :-> c :>>= d)
simpleExpression ((a :>>= b :-> c) :>> d)
    = simpleExpression (a :>>= b :-> c :>> d)
simpleExpression ((a :>> b) :>>= c)
    = simpleExpression (a :>> b :>>= c)
simpleExpression ((a :>> b) :>> c)
    = simpleExpression (a :>> b :>> c)
simpleExpression (Unit Empty :>> c)
    = simpleExpression c
simpleExpression (a :>> b)
    = do a' <- simpleExpression a
         b' <- simpleExpression b
         return (a' :>> b')
simpleExpression (a :>>= b :-> c)
    = do a' <- simpleExpression a
         c' <- simpleExpression c
         return (a' :>>= b :-> c')
simpleExpression (Application fn values)
    = liftM (Application fn) $ doSubsts values
simpleExpression (Store v)
    = liftM Store $ simpleValue v
simpleExpression (Unit value)
    = liftM Unit (simpleValue value)
simpleExpression (Case var [Variable v :> alt])
    = simpleExpression (Unit (Variable var) :>>= v :-> alt)
simpleExpression (Case val alts)
    = do val' <- doSubst val
         alts' <- mapM simpleAlt alts
         return $ Case val' alts'


simpleAlt :: Alt -> Opt Alt
simpleAlt (v :> e) = do e' <- simpleExpression e
                        return (v :> e')



simpleValue :: Value -> Opt Value
simpleValue (Variable v)
    = liftM Variable $ doSubst v
simpleValue (Node name ty missing args)
    = liftM (Node name ty missing) $ doSubsts args
simpleValue (Vector vs)
    = liftM Vector $ doSubsts vs
simpleValue v@Lit{}  = return v
simpleValue v@Hole{} = return v
simpleValue v@Empty  = return v

doSubst :: Renamed -> Opt Renamed
doSubst var
    = asks $ \m -> case Map.lookup var m of
                     Nothing     -> var
                     Just newVar -> newVar

doSubsts :: [Renamed] -> Opt [Renamed]
doSubsts = mapM doSubst

subst :: Renamed -> Renamed -> Opt a -> Opt a
subst name value = local $ Map.insert name value

