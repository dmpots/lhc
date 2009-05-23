{-# LANGUAGE OverloadedStrings #-}
module Grin.HPT.Lower
    ( lower
    ) where

import Grin.Types

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer


import Grin.HPT.Environment
import Grin.HPT.Solve

type M a = ReaderT HeapAnalysis (State Int) a

lower :: HeapAnalysis -> Grin -> Grin
lower hpt grin
    = evalState (runReaderT worker hpt) (grinUnique grin)
    where worker = do fns <- mapM lowerFuncDef (grinFunctions grin)
                      unique <- get
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
         HeapAnalysis hpt <- ask
         case Map.lookup (VarEntry a) hpt of
           Just (Rhs rhs) -> do let Rhs rhs' = mconcat [ hpt Map.! HeapEntry hp | Heap hp <- rhs ]
                                alts <- mapM (mkApplyAlt []) rhs'
                                v <- newVariable
                                u <- mkUpdate a f v rhs'
                                return $ Application (Builtin "fetch") [a] :>>= f :->
                                         Case f alts :>>= v :->
                                         u :>> -- Application (Builtin "update") [a,v] :>>
                                         Unit (Variable v)
           Nothing -> return $ Application (Builtin "urk") []
lowerExpression (Application (Builtin "apply") [a,b])
    = do HeapAnalysis hpt <- ask
         case Map.lookup (VarEntry a) hpt of
           Just (Rhs rhs) -> do alts <- mapM (mkApplyAlt [b]) rhs
                                return $ Case a alts
           Nothing -> return $ Application (Builtin "urk") []
lowerExpression (Application fn args)
    = return $ Application fn args
lowerExpression (Case scrut alts)
    = do alts' <- mapM lowerAlt alts
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

mkUpdate :: Renamed -> Renamed -> Renamed ->[RhsValue] -> M Expression
mkUpdate ptr scrut val tags
    = do fnTags <- sequence [ do args' <- replicateM (length args) newVariable
                                 return $ Node tag FunctionNode n args' | t@(Tag tag FunctionNode n args) <- tags, n == 0 ]
         constrTags <- sequence [ do args' <- replicateM (length args) newVariable
                                     return $ Node tag nt n args' | t@(Tag tag nt n args) <- tags, not (n == 0 && nt == FunctionNode) ]
         --let doUpdate = Case val [ tag :> Application (Builtin "update") [ptr,val] | tag <- constrTags ]
         let doUpdate = Application (Builtin "update") [ptr,val]
         if null fnTags || null constrTags
            then return $ Unit Empty
            else return $ doUpdate

mkApplyAlt :: [Renamed] -> RhsValue -> M Alt
mkApplyAlt extraArgs (Tag tag FunctionNode n argsRhs) | n == length extraArgs
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag FunctionNode n args :> Application tag (args ++ extraArgs)
mkApplyAlt extraArgs (Tag tag nt n argsRhs)
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag nt n args :> Unit (Node tag nt (n - length extraArgs) (args ++ extraArgs))
mkApplyAlt _ val = error $ "Grin.HPT.Lower.mkApplyAlt: expected tag: " ++ show val

newVariable :: M Renamed
newVariable = do unique <- get
                 put (unique + 1)
                 return $ Anonymous unique

