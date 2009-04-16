module Grin.Lowering.Primitives
    ( lower
    ) where

import CompactString
import Grin.Types

import Control.Monad.State

lower :: Int -> Grin -> Grin
lower u grin
    = evalState (lowerGrin grin) u


type Lower a = State Int a

lowerGrin :: Grin -> Lower Grin
lowerGrin grin
    = do defs <- mapM lowerFuncDef (grinFunctions grin)
         return grin{grinFunctions = defs}

lowerFuncDef :: FuncDef -> Lower FuncDef
lowerFuncDef def
    = do body <- lowerExpression (funcDefBody def)
         return def{funcDefBody = body}

lowerExpression :: Expression -> Lower Expression
lowerExpression (e :>>= lam)
    = do e' <- lowerExpression e
         lam' <- lowerLambda lam
         return $ e' :>>= lam'
--lowerExpression (Application (Builtin fn) [a,b]) | fn == fromString ">=#"
--    = do 
lowerExpression (Application (Builtin fn) []) | fn == fromString "realWorld#"
    = return $ Unit Empty
lowerExpression (Application fn vs)
    = return $ Application fn vs
lowerExpression (Case scrut alts)
    = do alts' <- mapM lowerLambda alts
         return $ Case scrut alts'
lowerExpression (Store v)
    = return $ Store v
lowerExpression (Unit v)
    = return $ Unit v

lowerLambda :: Lambda -> Lower Lambda
lowerLambda (v :-> e)
    = do e' <- lowerExpression e
         return $ v :-> e'


newVariable :: Lower Renamed
newVariable
    = do u <- get
         put (u+1)
         return $ Anonymous u

