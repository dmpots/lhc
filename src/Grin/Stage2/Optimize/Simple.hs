{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Grin.Stage2.Optimize.Simple
    ( optimize
    ) where

import Grin.Stage2.Types

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
simpleExpression (Unit v1 :>>= v2 :-> b)
    = do v1' <- doSubsts v1
         subst (zip v2 v1') (simpleExpression b)
simpleExpression (a :>>= v1 :-> Unit v2) | v1 == v2
    = simpleExpression a
simpleExpression ((a :>>= b :-> c) :>>= d)
    = simpleExpression (a :>>= b :-> c :>>= d)
simpleExpression (a :>>= b :-> c)
    = do a' <- simpleExpression a
         c' <- simpleExpression c
         return (a' :>>= b :-> c')
simpleExpression (Application fn values)
    = liftM (Application fn) $ doSubsts values
simpleExpression (Store vs)
    = liftM Store $ mapM doSubst vs
simpleExpression (Unit values)
    = liftM Unit (mapM doSubst values)
simpleExpression (Case var [_ :> alt])
    = simpleExpression alt
simpleExpression (Case val alts)
    = do val' <- doSubst val
         alts' <- mapM simpleAlt alts
         return $ Case val' alts'
simpleExpression (Fetch n p)
    = liftM (Fetch n) (doSubst p)
simpleExpression (Constant c)
    = return $ Constant c


simpleAlt :: Alt -> Opt Alt
simpleAlt (v :> e) = do e' <- simpleExpression e
                        return (v :> e')


doSubst :: Renamed -> Opt Renamed
doSubst var
    = asks $ \m -> case Map.lookup var m of
                     Nothing     -> var
                     Just newVar -> newVar

doSubsts :: [Renamed] -> Opt [Renamed]
doSubsts = mapM doSubst

subst :: [(Renamed, Renamed)] -> Opt a -> Opt a
subst pairs = local $ \m -> Map.fromList pairs `Map.union` m

