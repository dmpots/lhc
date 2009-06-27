{-# LANGUAGE OverloadedStrings #-}
module Grin.HPT.Solve
    ( HeapAnalysis(..)
    , solve
    ) where

import Grin.Types

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Writer

import Grin.HPT.Environment

data HeapAnalysis
    = HeapAnalysis (Map.Map Lhs Rhs)

instance Show HeapAnalysis where
    show (HeapAnalysis eqs)
        = unlines [ show lhs ++ " = " ++ show rhs | (lhs,rhs) <- Map.toList eqs ]

type SharingMap = Map.Map Lhs Bool

type M a = ReaderT (Equations,SharingMap) (Writer (Endo Equations, Endo SharingMap)) a


solve :: Equations -> (Int, HeapAnalysis)
solve eqs
    = let iterate ls
              = forM_ ls $ \(lhs,rhs) ->
                  do reducedRhs <- reduceEqs rhs
                     addReduced lhs reducedRhs
          loop iter shared prev
              = case execWriter (runReaderT (iterate (Map.toList eqs)) (prev, shared)) of
                  (newDefs, newShared) ->
                    let next = appEndo newDefs prev
                    in if prev == next then (iter, HeapAnalysis next) else loop (iter+1) (appEndo newShared shared) next
      in loop 1 (nonlinearVariables eqs) (Map.map (const mempty) eqs)

-- Scan for shared variables. A variable is shared if it is used more than once.
-- Detecting shared heap points is done later when we solve the equations.
nonlinearVariables :: Equations -> SharingMap
nonlinearVariables eqs
    = appEndo (execWriter (mapM_ rhsFn (Map.elems eqs))) Map.empty
    where rhsFn (Rhs values) = mapM_ worker values
          pushIdent ident = tell $ Endo $ Map.insertWith (\_ _ -> True) (VarEntry ident) False
          worker (Extract ident tag _nth)   = pushIdent ident >> pushIdent tag
          worker (ExtractVector ident _nth) = pushIdent ident
          worker (Eval ident)               = pushIdent ident
          worker (Update a b)               = pushIdent a >> pushIdent b
          worker (Apply a b)                = pushIdent a >> pushIdent b
          worker (PartialApply a b)         = return ()
          worker (Ident ident)              = pushIdent ident
          worker (Fetch ident)              = pushIdent ident
          worker Base                       = return ()
          worker Heap{}                     = return ()
          worker (Tag tag _nt _nargs args)  = pushIdent tag >> mapM_ rhsFn args
          worker (VectorTag args)           = mapM_ rhsFn args

addReduced :: Lhs -> Rhs -> M ()
addReduced lhs rhs
    = do orig <- lookupEq lhs
         unless (rhs `isSubsetOf` orig) $
           do tell (Endo $ Map.insertWith mappend lhs rhs, mempty)
              shared <- isShared lhs
              when shared $
                mapM_ setShared (listHeapPointers rhs)

listHeapPointers :: Rhs -> [HeapPointer]
listHeapPointers rhs = workerRhs rhs []
    where workerRhs (Rhs values)            = flip (foldr worker) values
          worker (Heap hp)                  = (hp:)
          worker (Tag _tag _nt _nargs args) = flip (foldr workerRhs) args
          worker (VectorTag args)           = flip (foldr workerRhs) args
          worker _                          = id


reduceEqs :: Rhs -> M Rhs
reduceEqs (Rhs rhs) = do rhs' <- mapM reduceEq rhs
                         return $ mconcat rhs'

reduceEq :: RhsValue -> M Rhs
reduceEq Base      = return $ singleton Base
reduceEq (Heap hp) = return $ singleton $ Heap hp
reduceEq (Ident i) = lookupEq (VarEntry i)
reduceEq (Extract eq tag n)
    = do Rhs eqs' <- lookupEq (VarEntry eq)
         return (mconcat [ args `nth` n | Tag t _ _ args <- eqs', t == tag ])
    where nth [] n = mempty --error $ "reduceEq: ExtractVector: " ++ show (eqs, tag, n)
          nth (x:xs) 0 = x
          nth (x:xs) n = nth xs (n-1)
reduceEq (ExtractVector eq n)
    = do Rhs eqs' <- lookupEq (VarEntry eq)
         return (mconcat [ args `nth` n | VectorTag args <- eqs' ])
    where nth [] n = error $ "reduceEq: ExtractVector: " ++ show (eq, n)
          nth (x:xs) 0 = x
          nth (x:xs) n = nth xs (n-1)
reduceEq (Tag t nt missing args)
    = do args' <- mapM reduceEqs args
         return $ singleton (Tag t nt missing args')
reduceEq (VectorTag args)
    = do args' <- mapM reduceEqs args
         return $ singleton (VectorTag args')
reduceEq (Eval i)
    = do Rhs vals <- lookupEq (VarEntry i)
         let unHeap (Heap hp) = hp
             unHeap t         = error $ "reduceEq: eval: " ++ show (t,i,vals)
             hps = map unHeap vals
         anyShared <- liftM or $ mapM (isShared . HeapEntry) hps
         let fn hp = do Rhs rhs <- lookupEq (HeapEntry hp)
                        let worker (Tag fn FunctionNode 0 _) = lookupEq (VarEntry fn)
                            worker other = return $ singleton other
                        rets <- liftM mconcat $ mapM worker rhs
                        when anyShared $ addReduced (HeapEntry hp) rets
                        return rets
         liftM mconcat $ mapM fn hps
reduceEq (Fetch i)
    = do Rhs vals <- lookupEq (VarEntry i)
         let f (Heap hp) = lookupEq (HeapEntry hp)
             f Base      = return mempty
             f t = error $ "reduceEq: fetch: " ++ show (t,i,vals)
         liftM mconcat $ mapM f vals
reduceEq (Apply a b)
    = do Rhs vals <- lookupEq (VarEntry a)
         let f (Tag func FunctionNode 1 args)
                 = reduceEq (Ident func)
             f (Tag conc nt n args)
                 | n == 0    = return mempty
                 | otherwise = do bRhs <- lookupEq (VarEntry b)
                                  return $ singleton (Tag conc nt (n-1) (args ++ [bRhs]))
             f t             = error $ "reduceEq: apply: " ++ show t
         liftM mconcat $ mapM f vals
reduceEq (PartialApply a b)
    = do Rhs vals <- lookupEq (VarEntry a)
         let f (Tag tag nt n args)
                 | n == 0    = return mempty
                 | otherwise = do bRhs <- lookupEq (VarEntry b)
                                  return $ singleton (Tag tag nt (n-1) (args ++ [bRhs]))
             f t             = error $ "reduceEq: apply: " ++ show t
         liftM mconcat $ mapM f vals
reduceEq (Update hp val)
    = do Rhs hps <- lookupEq (VarEntry hp)
         valRhs  <- lookupEq (VarEntry val)
         forM_ hps $ \(Heap hp) -> addReduced (HeapEntry hp) valRhs
         return mempty

-- FIXME: Throw an exception if 'lhs' couldn't be found.
lookupEq :: Lhs -> M Rhs
lookupEq lhs
    = asks $ \(eqs, _sharingMap) -> Map.findWithDefault mempty lhs eqs

-- FIXME: Throw an exception if 'lhs' couldn't be found.
isShared :: Lhs -> M Bool
isShared lhs
    = asks $ \(_eqs, sharingMap) -> Map.findWithDefault False lhs sharingMap

setShared :: HeapPointer -> M ()
setShared hp = tell (mempty, Endo $ Map.insert (HeapEntry hp) True)

