{-# LANGUAGE OverloadedStrings #-}
module Grin.HPT.Lower
    ( lower
    ) where

import Grin.Types

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Writer
import Data.List (delete)


import Grin.HPT.Environment
import Grin.HPT.Solve

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
         HeapAnalysis hpt sharingMap <- gets fst
         case Map.lookup (VarEntry a) hpt of
           Just (Rhs rhs) -> do let Rhs rhs' = mconcat [ hpt Map.! HeapEntry hp | Heap hp <- rhs ]
                                addHPTInfo (VarEntry f) (Rhs rhs')
                                alts <- mapM (mkApplyAlt rhs []) rhs'
                                v <- newVariable
                                let expand (Tag tag FunctionNode 0 _) = hpt Map.! (VarEntry tag)
                                    expand rhs = Rhs [rhs]
                                    expanded = mconcat $ map expand rhs'
                                addHPTInfo (VarEntry v) expanded
                                let anyShared = or [ Map.findWithDefault False (HeapEntry hp) sharingMap | Heap hp <- rhs ]
                                u <- mkUpdate anyShared a f v rhs' expanded
                                return $ Application (Builtin "fetch") [a] :>>= f :->
                                         Case f alts :>>= v :->
                                         u :>>
                                         Unit (Variable v)
           Nothing -> return $ Application (Builtin "urk") []
lowerExpression (Application (Builtin "apply") [a,b])
    = do HeapAnalysis hpt _ <- gets fst
         case Map.lookup (VarEntry a) hpt of
           Just (Rhs rhs) -> do alts <- mapM (mkApplyAlt [] [b]) rhs
                                return $ Case a alts
           Nothing -> return $ Application (Builtin "urk") []
lowerExpression (Application fn args)
    = return $ Application fn args
lowerExpression (Case scrut alts)
    = do HeapAnalysis hpt _ <- gets fst
         case Map.lookup (VarEntry scrut) hpt of
           Just (Rhs rhs) -> do alts' <- mapM lowerAlt (filter (`isMemberOf` rhs) alts)
                                return $ Case scrut alts'
           Nothing -> error "Grin.HPT.Lower.lowerExpression: Urk"
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

(Node tag nt missing args :> _) `isMemberOf` rhs
    = (tag, nt, missing) `elem` [ (tag, nt, missing) | Tag tag nt missing _ <- rhs ]
_ `isMemberOf` rhs = True


mkUpdate :: Bool -> Renamed -> Renamed -> Renamed ->[RhsValue] -> Rhs -> M Expression
mkUpdate False ptr scrut val tags _ = return $ Unit Empty
mkUpdate shared ptr scrut val tags (Rhs expanded)
    = do let doUpdate = do alts <- mapM uWorker expanded
                           return $ Case val alts
             uWorker (Tag tag nt missing args)
                 = do args' <- replicateM (length args) newVariable
                      node <- newVariable
                      addHPTInfo (VarEntry node) (singleton $ Tag tag nt missing args)
                      return (Node tag nt missing args' :> (Unit (Node tag nt missing args') :>>= node :->
                              Application (Builtin "update") [ptr,node]))
         let worker (Tag tag FunctionNode 0 args)
                 = do args' <- replicateM (length args) newVariable
                      u <- doUpdate
                      return $ Node tag FunctionNode 0 args' :> u
             worker (Tag tag nt missingArgs args)
                 = do args' <- replicateM (length args) newVariable
                      return $ Node tag nt missingArgs args' :> Unit Empty
             worker tag = error $ "Grin.HPT.Lower.mkUpdate: Unknown rhs value: " ++ show tag
         alts' <- mapM worker tags
         return $ Case scrut alts'

mkApplyAlt :: [RhsValue] -> [Renamed] -> RhsValue -> M Alt
mkApplyAlt _ extraArgs (Tag tag FunctionNode n argsRhs) | n == length extraArgs
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag FunctionNode n args :> Application tag (args ++ extraArgs)
mkApplyAlt _ extraArgs (Tag tag nt n argsRhs)
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag nt n args :> Unit (Node tag nt (n - length extraArgs) (args ++ extraArgs))
mkApplyAlt _ _ val = error $ "Grin.HPT.Lower.mkApplyAlt: unexpected tag: " ++ show val

addHPTInfo :: Lhs -> Rhs -> M ()
addHPTInfo lhs rhs
    = modify $ \(HeapAnalysis hpt smap, unique) -> (HeapAnalysis (Map.insertWith mappend lhs rhs hpt) smap, unique)

newVariable :: M Renamed
newVariable = do unique <- gets snd
                 modify $ \st -> (fst st, unique + 1)
                 return $ Anonymous unique

