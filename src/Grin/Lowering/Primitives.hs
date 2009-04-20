{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Grin.Lowering.Primitives
    ( lower
    ) where

import CompactString
import Grin.Types

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

lower :: Int -> Grin -> Grin
lower u grin
    = evalState (runReaderT (lowerGrin grin) emptyScope) u
    where emptyScope = Scope { nodeMap = Map.fromList [ (name, node) | node <- grinNodes grin, Just name <- [alias (nodeName node)]] }


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
lowerExpression (Application (Builtin "newMVar#") [realWorld])
    = do v <- newVariable
         return $ Store Empty :>>= v :-> Unit (Vector [realWorld, v])
lowerExpression (Application (Builtin "putMVar#") [ptr, val, realWorld])
    = return $ Application (Builtin "update") [ptr, val] :>>= Empty :-> Unit realWorld
lowerExpression (Application (Builtin "takeMVar#") [ptr, realWorld])
    = do v <- newVariable
         return $ Application (Builtin "fetch") [ptr] :>>= v :-> Unit (Vector [realWorld, v])
lowerExpression (Application (Builtin "readWorld#") [])
    = return $ Unit Empty -- FIXME: Use a special RealWorld value?
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


renamedOpts = [ ("gtChar#", ">#")
              , ("geChar#", ">=#")
              , ("ltChar#", "<#")
              , ("leChar#", "<=#")
              ]



lookupNode :: CompactString -> Lower Renamed
lookupNode name
    = do m <- asks nodeMap
         case Map.lookup name m of
           Just node -> return (nodeName node)
           Nothing   -> error $ "Couldn't find node: " ++ show name

newVariable :: Lower Value
newVariable
    = do u <- get
         put (u+1)
         return $ Variable $ Anonymous u

