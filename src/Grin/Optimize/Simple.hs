{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Grin.Optimize.Simple
    ( optimize
    ) where

import Grin.Types

import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Traverse

type Opt a = Reader Subst a
type Subst = Map.Map Renamed Renamed


optimize :: Grin -> Grin
optimize grin
    = grin{ grinFunctions = map simpleFuncDef (grinFunctions grin)}


simpleFuncDef :: FuncDef -> FuncDef
simpleFuncDef def
    = let simplified = runReader (simpleExpression (funcDefBody def)) Map.empty
          fetched    = runReader (fetchOpt simplified) Map.empty
          pruned     = runReader (casePruneOpt fetched) Map.empty
      in def{ funcDefBody = pruned }

simpleExpression :: Expression -> Opt Expression
simpleExpression (Unit (Variable v1) :>>= v2 :-> t)
    = do v1' <- doSubst v1
         subst v2 v1' (simpleExpression t)
simpleExpression (a :>>= v1 :-> Unit (Variable v2)) | v1 == v2
    = simpleExpression a
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
simpleExpression (Case var [])
    = return $ Application (Builtin "unreachable") []
simpleExpression (Case var [Variable v :> alt])
    = simpleExpression (Unit (Variable var) :>>= v :-> alt)
simpleExpression (Case var alts) | and [ case alt of Unit ret -> ret == cond; _ -> False | cond :> alt <- alts]
    = simpleExpression (Unit (Variable var))
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




-- do p <- store x 
--    y <- fetch p
--    m
--  >>>
-- do y <- unit x
--    m
type FetchOpt a = Reader Heap a
type Heap = Map.Map (Either Renamed Renamed) Expression

fetchOpt :: Expression -> FetchOpt Expression
fetchOpt e@(Store val :>>= bind :-> _)
    = local (Map.insert (Left bind) (Unit val))
            (tmapM fetchOpt e)
fetchOpt e@(Application (Builtin "fetch") [val] :>>= bind :-> _)
    = local (Map.insert (Right bind) (Unit (Variable val)))
            (tmapM fetchOpt e)
fetchOpt e@(Application (Builtin "update") [ptr,val] :>> _)
    = local (Map.insert (Left ptr) (Unit (Variable val)))
            (tmapM fetchOpt e)
fetchOpt e@(Application (Builtin "fetch") [ptr])
    = do mbVal <- asks $ Map.lookup (Left ptr)
         case mbVal of
           Nothing -> return e
           Just e' -> return e'
fetchOpt e@(Store (Variable val))
    = do mbVal <- asks $ Map.lookup (Right val)
         case mbVal of
           Nothing -> return e
           Just e' -> return e'
fetchOpt e = tmapM fetchOpt e



type CasePruneOpt a = Reader Cases a
type Cases = Map.Map Renamed (Set.Set Renamed)

casePruneOpt :: Expression -> CasePruneOpt Expression
casePruneOpt e@(Unit (Node tag _ _ _) :>>= v :-> _)
    = local (Map.insertWith Set.union v (Set.singleton tag))
            (tmapM casePruneOpt e)
casePruneOpt e@(Case scrut alts)
    = do mbVals <- asks $ Map.lookup scrut
         case mbVals of
           Nothing   -> tmapM casePruneOpt e
           Just vals -> let worker (Node tag _ _ _ :> _) | tag `Set.notMember` vals = Nothing
                            worker alt = Just alt
                        in tmapM casePruneOpt (Case scrut (mapMaybe worker alts))
casePruneOpt e = tmapM casePruneOpt e


