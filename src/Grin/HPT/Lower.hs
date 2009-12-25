{-# LANGUAGE OverloadedStrings #-}
module Grin.HPT.Lower
    ( lower
    ) where

import Grin.Types as Grin

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Writer
import Data.List (delete)


import Grin.HPT.Environment (Lhs(..))
import Grin.HPT.Interface as Interface

type M a = State (HeapAnalysis, Int) a

lower :: HeapAnalysis -> Grin -> (Grin, HeapAnalysis)
lower hpt grin
    = case runState worker (hpt, grinUnique grin) of
        (grin, (hpt',_newUnique)) -> (grin, hpt')
    where worker = do fns <- mapM lowerFuncDef (grinFunctions grin)
                      unique <- gets snd
                      return grin{ grinFunctions = fns
                                 , grinUnique    = unique }

lowerFuncDef :: FuncDef -> M FuncDef
lowerFuncDef func
    = do body <- lowerExpression (funcDefBody func)
         return $ func{funcDefBody = body}

lowerExpression :: Expression -> M Expression
lowerExpression (a :>>= lam)
    = do a' <- lowerExpression a
         lam' <- lowerLambda lam
         return $ a' :>>= lam'
lowerExpression (a :>> b)
    = do a' <- lowerExpression a
         b' <- lowerExpression b
         return $ a' :>> b'
lowerExpression (Application (Builtin "eval") [a])
    = do f <- newVariable
         hpt <- gets fst
         case lookupHeap a hpt of
           Interface.Empty -> return $ Application (Builtin "unreachable") []
           rhs@Other{rhsTagged = nodes}
             -> do let tags = Map.toList nodes
                   addHPTInfo (VarEntry f) rhs -- (Tagged nodes)
                   alts <- mapM (mkApplyAlt []) tags
                   v <- newVariable
                   let expand ((tag,FunctionNode,0),_args) = lookupLhs (VarEntry tag) hpt
                       expand (node,args) = Other (Map.singleton node args) []
                       expanded = mconcat $ map expand tags
                   addHPTInfo (VarEntry v) expanded
                   let anyShared = heapIsShared a hpt
                   u <- mkUpdate anyShared a f v tags expanded
                   return $ Application (Builtin "fetch") [a] :>>= f :->
                            Case f alts :>>= v :->
                            u :>>
                            Unit (Variable v)
lowerExpression (Application (Builtin "apply") [a,b])
    = do hpt <- gets fst
         case lookupLhs (VarEntry a) hpt of
           Other{rhsTagged = nodes} -> do alts <- mapM (mkApplyAlt [b]) (Map.toList nodes)
                                          return $ Case a alts
           Interface.Empty -> return $ Application (Builtin "unreachable") []
lowerExpression (Application fn args)
    = return $ Application fn args
lowerExpression (Update size ptr val)
    = return $ Update size ptr val
lowerExpression (Case scrut alts)
    = do hpt <- gets fst
         let rhs = lookupLhs (VarEntry scrut) hpt
         alts' <- mapM lowerAlt (filter (`isMemberOf` rhs) alts)
         return $ Case scrut alts'
lowerExpression (Store val)
    = return $ Store val
lowerExpression (Unit val) = return $ Unit val

lowerLambda :: Lambda -> M Lambda
lowerLambda (a :-> b)
    = do b' <- lowerExpression b
         return $ a :-> b'

lowerAlt :: Alt -> M Alt
lowerAlt (a :> b)
    = do b' <- lowerExpression b
         return $ a :> b'

(Node tag nt missing args :> _) `isMemberOf` (Other{rhsTagged = nodes})
    = (tag, nt, missing) `Map.member` nodes
_ `isMemberOf` rhs = True


mkUpdate :: Bool -> Renamed -> Renamed -> Renamed ->[(Node, [Rhs])] -> Rhs -> M Expression
mkUpdate False ptr scrut val tags _ = return $ Unit Grin.Empty
mkUpdate shared ptr scrut val tags _ -- (Other{rhsTagged = expanded})
    = do hpt <- gets fst
         let doUpdate tag = case lookupLhs (VarEntry tag) hpt of
                              Other{rhsTagged = expanded} -> do alts <- mapM (uWorker val) (Map.toList expanded)
                                                                return $ Case val alts
                              _ -> return $ Unit Grin.Empty
             uWorker val ((tag, nt, missing), args)
                  = do args' <- replicateM (length args) newVariable
                       node <- newVariable
                       addHPTInfo (VarEntry node) (Other (Map.singleton (tag, nt, missing) args) [])
                       return (Node tag nt missing args' :> Update (length args'+1) ptr val)
         let worker ((tag, FunctionNode, 0), args)
                 = do args' <- replicateM (length args) newVariable
                      u <- doUpdate tag
                      return $ Node tag FunctionNode 0 args' :> u
             worker ((tag, nt, missingArgs), args)
                 = do args' <- replicateM (length args) newVariable
                      return $ Node tag nt missingArgs args' :> Unit Grin.Empty
         alts' <- mapM worker tags
         return $ Case scrut alts'
mkUpdate shared ptr scrut val tags _ = return $ Unit Grin.Empty

mkApplyAlt :: [Renamed] -> (Node, [Rhs]) -> M Alt
mkApplyAlt extraArgs ((tag, FunctionNode, n), argsRhs) | n == length extraArgs
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag FunctionNode n args :> Application tag (args ++ extraArgs)
mkApplyAlt [extraArg] ((tag, nt, 0), argsRhs)
    = return $ Node tag nt 0 [] :> Application (Builtin "unreachable") []
mkApplyAlt extraArgs ((tag, nt, n), argsRhs)
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag nt n args :> Unit (Node tag nt (n - length extraArgs) (args ++ extraArgs))
mkApplyAlt _ val = error $ "Grin.HPT.Lower.mkApplyAlt: unexpected tag: " ++ show val

addHPTInfo :: Lhs -> Rhs -> M ()
addHPTInfo lhs rhs
    = modify $ \(hpt, unique) -> (hptAddBinding lhs rhs hpt, unique)

newVariable :: M Renamed
newVariable = do unique <- gets snd
                 modify $ \st -> (fst st, unique + 1)
                 return $ Anonymous unique

