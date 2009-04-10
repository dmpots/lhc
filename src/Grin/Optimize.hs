{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Grin.Optimize
    ( simpleOptimize
    ) where

import Grin.Types
import Grin.Pretty

import Control.Monad.Reader
import qualified Data.Map as Map

import Debug.Trace


newtype Opt a = Opt {unOpt :: Reader Subst a}
    deriving (MonadReader Subst, Monad)
type Subst = Map.Map Renamed Value


simpleOptimize :: Grin -> Grin
simpleOptimize grin
    = grin{ grinFunctions = map simpleFuncDef (grinFunctions grin)}


simpleFuncDef :: FuncDef -> FuncDef
simpleFuncDef def
    = def{ funcDefBody = runReader (unOpt (simpleExpressionDeep (funcDefBody def))) Map.empty }

simpleExpressionDeep :: Expression -> Opt Expression
simpleExpressionDeep (e :>>= v :-> t)
    = do e' <- simpleExpressionDeep e
         simpleExpression (e' :>>= v :-> t)
simpleExpressionDeep e = simpleExpression e

simpleExpression :: Expression -> Opt Expression
simpleExpression (Unit value :>>= Variable v :-> t)
    = do t' <- subst v value (simpleExpressionDeep t)
         return t' -- (Unit value :>>= Variable v :-> t')
simpleExpression (a :>>= v :-> Unit v') | v == v'
    = return a
simpleExpression (a :>>= b :-> c)
    = do c' <- simpleExpressionDeep c
         return (a :>>= b :-> c')
simpleExpression (Application fn values)
    = do vals <- mapM simpleValue values
         return $ Application fn vals
simpleExpression (Store v)
    = do v' <- simpleValue v
         return $ Store v'
simpleExpression (Unit value)
    = liftM Unit (simpleValue value)
simpleExpression (Case val alts)
    = do val' <- simpleValue val
         alts' <- mapM simpleLambda alts
         return $ Case val' alts'
simpleExpression e = return e


simpleLambda :: Lambda -> Opt Lambda
simpleLambda (v :-> e) = do e' <- simpleExpressionDeep e
                            return (v :-> e')



simpleValue :: Value -> Opt Value
simpleValue (Variable v)
    = do m <- ask
         case Map.lookup v m of
           Nothing     -> return $ Variable v
           Just newVal -> return newVal
simpleValue (Node name ty args)
    = do args' <- mapM simpleValue args
         return $ Node name ty args'
simpleValue v = return v

subst :: Renamed -> Value -> Opt a -> Opt a
subst name val
    = local $ Map.insert name val


