{-# LANGUAGE OverloadedStrings #-}
module Grin.HPT.Solve
    ( HeapAnalysis(..)
    , solve
    ) where

import Grin.Types

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Writer

--import System.IO
--import System.IO.Unsafe

import Grin.HPT.Environment

data HeapAnalysis
    = HeapAnalysis (Map.Map Lhs Rhs)


type M a = ReaderT Equations (Writer (Endo Equations)) a


solve :: Equations -> (Int, HeapAnalysis)
solve eqs
    = let iterate ls
              = forM_ ls $ \(lhs,rhs) ->
                  do reducedRhs <- reduceEqs rhs
                     addReduced lhs reducedRhs
          loop iter prev
              = case {-traceOut ("\nIteration: " ++ show iter ++ "\n") $-} (execWriter (runReaderT (iterate (Map.toList eqs)) prev)) of
                  newDefs ->
                    let next = (Map.unionWith mappend prev (appEndo newDefs Map.empty))
                    in if prev == next then (iter, HeapAnalysis next) else loop (iter+1) next
      in loop 1 (Map.map (const mempty) eqs)

--traceOut str v = unsafePerformIO (putStr str) `seq` v


isSubsetOf :: (Monoid a, Eq a) => a -> a -> Bool
a `isSubsetOf` b = b == (a `mappend` b)

addReduced :: Lhs -> Rhs -> M ()
addReduced lhs rhs
    = do orig <- lookupEq lhs
         {-let isNew = not (rhs `isSubsetOf` orig)
               tag = if isNew then "+" else "-"
         traceOut tag $-}
         unless (rhs `isSubsetOf` orig) $ tell $ Endo $ Map.insertWith mappend lhs rhs

reduceEqs :: Rhs -> M Rhs
reduceEqs (Rhs rhs) = do rhs' <- mapM reduceEq rhs
                         return $ mconcat rhs'

reduceEq :: RhsValue -> M Rhs
reduceEq Base      = return $ singleton Base
reduceEq (Heap hp) = return $ singleton $ Heap hp
reduceEq (Ident i) = lookupEq (VarEntry i)
reduceEq (Extract eq tag n)
    = do Rhs eqs' <- lookupEq (VarEntry eq)
         reduceEqs (mconcat [ args `nth` n | Tag t _ _ args <- eqs', t == tag ])
    where nth [] n = mempty --error $ "reduceEq: ExtractVector: " ++ show (eqs, tag, n)
          nth (x:xs) 0 = x
          nth (x:xs) n = nth xs (n-1)
reduceEq (ExtractVector eq n)
    = do Rhs eqs' <- lookupEq (VarEntry eq)
         reduceEqs (mconcat [ args `nth` n | VectorTag args <- eqs' ])
    where nth [] n = error $ "reduceEq: ExtractVector: " ++ show (eq, n)
          nth (x:xs) 0 = x
          nth (x:xs) n = nth xs (n-1)
{-
reduceEq (Tag fn FunctionNode 0 args)
    = do args' <- mapM reduceEqs args
         rets <- lookupEq (VarEntry fn)
         return $ singleton (Tag fn FunctionNode 0 args') `mappend` rets
-}
reduceEq (Tag t nt missing args)
    = do --args' <- mapM reduceEqs args
         return $ singleton (Tag t nt missing args)
reduceEq (VectorTag args)
    = do args' <- mapM reduceEqs args
         return $ singleton (VectorTag args')
reduceEq (Eval i)
    = do Rhs vals <- lookupEq (VarEntry i)
         let f (Heap hp) = do Rhs rhs <- lookupEq (HeapEntry hp)
                              let worker (Tag fn FunctionNode 0 _) = lookupEq (VarEntry fn)
                                  worker other = return $ singleton other
                              rets <- liftM mconcat $ mapM worker rhs
                              addReduced (HeapEntry hp) rets
                              return rets
             f t = error $ "reduceEq: eval: " ++ show (t,i,vals)
         liftM mconcat $ mapM f vals
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
                 | otherwise = return $ singleton (Tag conc nt (n-1) (args ++ [singleton (Ident b)]))
             f t             = error $ "reduceEq: apply: " ++ show t
         liftM mconcat $ mapM f vals
reduceEq (PartialApply a b)
    = do Rhs vals <- lookupEq (VarEntry a)
         let f (Tag tag nt n args)
                 | n == 0    = return mempty
                 | otherwise = return $ singleton (Tag tag nt (n-1) (args ++ [singleton (Ident b)]))
             f t             = error $ "reduceEq: apply: " ++ show t
         liftM mconcat $ mapM f vals
reduceEq (Update hp val)
    = do Rhs hps <- lookupEq (VarEntry hp)
         valRhs  <- lookupEq (VarEntry val)
         forM_ hps $ \(Heap hp) -> addReduced (HeapEntry hp) valRhs
         return mempty

lookupEq :: Lhs -> M Rhs
lookupEq lhs
    = asks $ \eqs -> Map.findWithDefault mempty lhs eqs



