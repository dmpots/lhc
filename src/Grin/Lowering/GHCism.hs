{-# LANGUAGE OverloadedStrings, PatternGuards #-}
{- |
  The External Core output from GHC suffers from many GHCisms and the direct
  translation to GRIN results in incorrect code.
  This module clears out GHC specific patterns and returns GRIN code fit for use.
  CAUTION: This pass MUST be run exactly once from Grin.FromCore.
           Any deviation will mess things up.
-}
module Grin.Lowering.GHCism
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

data Scope = Scope { nodeMap :: Map.Map CompactString NodeDef }
type Lower a = ReaderT Scope (State Int) a

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
lowerExpression (Application (Builtin fn) [a,b]) | Just renamed <- lookup fn renamedOpts
    = lowerExpression (Application (Builtin renamed) [a,b])
lowerExpression (Application (Builtin fn) [a,b]) | fn `elem` [">=#",">#","==#","<=#","<#"]
    = do tnode <- lookupNode $ fromString "ghc-prim:GHC.Bool.True"
         fnode <- lookupNode $ fromString "ghc-prim:GHC.Bool.False"
         v <- newVariable
         return $ Application (Builtin fn) [a,b] :>>= v :-> Case v [Lit (Lint 0) :-> Unit (Node fnode (ConstructorNode 0) [])
                                                                   ,Lit (Lint 1) :-> Unit (Node tnode (ConstructorNode 0) [])]
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
lowerExpression (Application (Builtin "int2Word#") [v])
    = return $ Unit v
lowerExpression (Application (Builtin "word2Int#") [v])
    = return $ Unit v
lowerExpression (Application (Builtin "plusAddr#") [a,b])
    = return $ Application (Builtin "+#") [a,b]
lowerExpression (Application (Builtin "eqAddr#") [a,b])
    = lowerExpression $ Application (Builtin "==#") [a,b]
lowerExpression (Application (Builtin fn) [a]) | fn `elem` ["chr#", "ord#"]
    = return $ Unit a
lowerExpression (Application (External external) args)
    = do v <- newVariable
         return $ Application (External external) (init args) :>>= v :-> Unit (Vector [last args, v])
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

