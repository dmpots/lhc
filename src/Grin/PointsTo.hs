{-# LANGUAGE BangPatterns #-}
module Grin.PointsTo
  ( Equation(..)
  , Equations
  , pointsTo
  , varToCaseStmts
  , genApplyStmts
  ) where

import Grin.Grin
--import Name.Id
import Doc.PPrint
import StringTable.Atom

import Data.List (nub,sort,isInfixOf)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Set as Set
import Control.Monad
import Text.Printf
import Data.Monoid

import Debug.Trace

data Equation = Extract [Equation] Tag Int
              | Eval Var
              | Apply Var Var
              | PartialApply Var Var
              | Ident Var
              | Base
              | Heap HeapPointer
              | Tag Atom [[Equation]]
                deriving (Show,Eq,Ord)

type HeapPointer = Int

data EqPointer = Equation Var | HeapPointer HeapPointer deriving (Show,Eq,Ord)

type Equations = Map.Map EqPointer [Equation]

data PointsTo = PointsTo Equations

eqUnion [] lst = lst
eqUnion lst [] = lst
eqUnion (Tag tag1 args1:xs) (Tag tag2 args2:ys)
    = case tag1 `compare` tag2 of
        LT -> Tag tag1 args1 : eqUnion xs (Tag tag2 args2:ys)
        GT -> Tag tag2 args2 : eqUnion (Tag tag1 args1:xs) ys
        EQ -> Tag tag1 [ eqUnion arg1 arg2 | (arg1,arg2) <- zip args1 args2 ]:eqUnion xs ys
eqUnion (y@Tag{}:ys) (x:xs)
    = x:eqUnion (y:ys) xs
eqUnion (y:ys) (x@Tag{}:xs)
    = y:eqUnion ys (x:xs)
eqUnion (y:ys) (x:xs)
    = case y `compare` x of
        LT -> y:eqUnion ys (x:xs)
        GT -> x:eqUnion (y:ys) xs
        EQ -> x:eqUnion ys xs

eqUnions = foldr eqUnion []

varToCaseStmts :: PointsTo -> Grin -> Var -> [Lam]
varToCaseStmts (PointsTo eqs) grin var
    = case Map.lookup (Equation var) eqs of
        Nothing   -> [ [Var (V 0) TyNode ] :-> Error "empty" [TyNode]] -- error $ "Grin.PointsTo.varToCaseStmt: this should not happen: " ++ show var
        Just vals -> [ tagToCaseStmt grin t | Tag t args <- vals ] ++
                    [ [Var (V 0) TyNode] :-> Error ("case fell off: "++show (var,vals)) [TyNode]]

tagToCaseStmt :: Grin -> Atom -> Lam
tagToCaseStmt grin tag
  = case tagUnfunction tag of
      Just (0,fn) -> [NodeC tag vars] :-> App fn vars [TyNode]
      _           -> [NodeC tag vars] :-> Return [NodeC tag vars]
  where Just tyty = findTyTy (grinTypeEnv grin) tag
        vars = flip map (zip [1000000,1000002..] (tySlots tyty)) $ \(n,ty) ->
                 Var (V n) ty

genApplyStmts :: PointsTo -> Grin -> Var -> Val -> [Ty] -> [Lam]
genApplyStmts (PointsTo eqs) grin var val ty
    = case Map.lookup (Equation var) eqs of
        Nothing   -> [ [Var (V 0) TyNode] :-> Error "empty apply" ty]
        Just vals -> [ appToCaseStmt grin t val ty | Tag t _ <- vals ] ++
                    [ [Var (V 0) TyNode] :-> Error "app fell off" ty]

appToCaseStmt grin tag (Var v ty) retTy
    = case tagUnfunction tag of
        Just (0,fn) -> error "fully applied function"
        Just (1,fn) -> [NodeC tag vars] :-> App fn newVars retTy
        Just (n,fn) -> [NodeC tag vars] :-> Return [NodeC (partialTag fn (n-1)) newVars]
        Nothing     -> error "apply on non-function"
  where Just tyty = findTyTy (grinTypeEnv grin) tag
        vars = flip map (zip [1000000,1000002..] (tySlots tyty)) $ \(n,ty) ->
                 Var (V n) ty
        newVars = vars ++ if ty == TyUnit then [] else [Var v ty]

applications = V (fromAtom $ toAtom "APPLICATIONS")

pointsTo grin
  = do let loop [] = return ()
           loop ((name,args :-> body):fs)
             = do ret <- setupEnv body
                  atomToVar name "_result" =: ret
                  when ("index" `isInfixOf` show name) $ trace (show name ++ ": " ++ show ret) $ return ()
                  sequence_ [ arg =: [Extract [Ident applications] (tagFlipFunction name) n] | (Var arg _, n) <- zip args [0..] ]
                  loop fs
           funcArgs = Map.fromList [ (name, args) | (name, args :-> _body) <- grinFuncs grin ]
       let (_,eqs, _n) = unGenEnv (loop (grinFuncs grin)) funcArgs Map.empty 0
       putStrLn "Raw"
       showEnv eqs
       let eqs' = solve eqs
       putStrLn "Solved"
       showEnv eqs'
       return $ PointsTo eqs'

newtype GenEnv a = GenEnv { unGenEnv :: Map.Map Atom [Val] -> Equations -> Int -> (a,Equations, Int) }

instance Monad GenEnv where
  f >>= g = GenEnv $ \fns eqs n ->
              case unGenEnv f fns eqs n of
                (a, eqs', n') -> unGenEnv (g a) fns eqs' n'
  return a = GenEnv $ \_fns eqs n -> (a, eqs, n)

--setupEnv :: Exp -> GenEnv ()
setupEnv (Store val)
  = do heapPointer <- store =<< processVal val
       return [Heap heapPointer]
setupEnv (App func [Var arg _] _) | func == funcEval
  = do return [Eval arg]
setupEnv (App func [Var arg1 _, Var arg2 _] _) | func == funcApply
  = do applications =: [PartialApply arg1 arg2]
       return [Apply arg1 arg2]
setupEnv (App func [Var arg1 _] _) | func == funcApply
  = do let arg2 = V 0
       applications =: [PartialApply arg1 arg2]
       return [Apply arg1 arg2]
setupEnv (App func args _)
  = do funcArgs <- lookupFuncArgs func
       forM_ (zip funcArgs args) $ \(Var var _, arg) ->
         do prim <- processVal arg
            var =: prim
       return [Ident (atomToVar func "_result")]
setupEnv (Prim _ _args ty) = return [Base]
setupEnv (Case val alts)
  = do prim <- processVal val
       rets <- forM alts $ \([l] :-> alt) ->
         case l of
           NodeC tag args -> do forM_ (zip [0..] args) $ \(n,Var arg _) ->
                                  arg =: [Extract prim tag n]
                                setupEnv alt
           Lit{}          -> setupEnv alt
           Var v ty       -> setupEnv alt
           _              -> error $ "Case: " ++ show l
       return $ eqUnions rets
setupEnv (Return vals)
  = do prims <- mapM processVal vals
       return $ eqUnions prims
setupEnv (Update (Var v1 _) v2)
  = do p2 <- processVal v2
       v1 =: p2
       return $ []
setupEnv (exp :>>= binds :-> rest)
  = do b <- setupEnv exp
       forM_ binds $ \bind ->
         case bind of
           Var l _        -> l =: b
           NodeC tag args -> forM_ (zip args [0..]) $ \(Var arg _,n) ->
                              arg =: [Extract b tag n]
       setupEnv rest
setupEnv (exp :>>= [] :-> rest)
  = do setupEnv exp
       setupEnv rest
setupEnv Error{} = return []
setupEnv (Fetch val) = processVal val
setupEnv body = error (show body) -- return (Set [NotDone])

processVal (NodeC tag args) = do args' <- mapM processVal args
                                 funcArgs <- lookupFuncArgs (tagFlipFunction tag)
                                 when (tagIsSuspFunction tag) $
                                   forM_ (zip funcArgs args') $ \(Var var _, value) ->
                                     var =: value
                                 return [Tag (fromAtom tag) args']
processVal (Var var _type) = return [Ident var]
processVal (Lit _ ty) = return [Base]
processVal (Const val) = processVal val
processVal Index{} = return []
processVal (ValPrim _ args _) = liftM concat $ mapM processVal args
processVal val = error $ "unsuported val: " ++ show val

store prim
  = GenEnv $ \_fns eqs n ->
    (n, Map.insertWith eqUnion (HeapPointer n) prim eqs , n+1)

(V 0) =: p = return ()
var =: p
  = GenEnv $ \_fns eqs n ->
    ((), Map.insertWith eqUnion (Equation var) p eqs, n)




lookupFuncArgs :: Atom -> GenEnv [Val]
lookupFuncArgs func
  = GenEnv $ \fns eqs n ->
    let ret = case Map.lookup func fns of
                Nothing   -> []
                Just args -> args
    in (ret, eqs,  n)

atomToVar atom suffix
  = V $ fromAtom $ toAtom $ fromAtom atom ++ suffix

showEnv :: Equations -> IO ()
showEnv eqs
  = do putStrLn "Equations:"
       forM_ (Map.toList eqs) $ \(var, eq) ->
         printf "%s -> %s\n" (show var :: String) (show eq)







-------------------------------------------------------------
-- Simplify

newtype Simplify a = Simplify { runSimplify :: Equations -> (a, Equations) }

instance Monad Simplify where
  f >>= g = Simplify $ \eqs ->
            case runSimplify f eqs of
              (a, eqs') -> runSimplify (g a) eqs'
  return a = Simplify $ \eqs -> (a, eqs)

inspect = [] -- [ Equation (V n) | n <- [906,101960,1151883282,101824,267838397211878910,-145367593]]

solve :: Equations -> Equations
solve eqs
    = let iterate [] = return ()
          iterate (var:xs)
              = do orig    <- lookupEq var
                   reduced <- reduceEqs (eqs ! var)
                   let new = eqUnion orig reduced
                   updateEq var new
                   iterate xs
          loop prev = case runSimplify (iterate (Map.keys eqs)) prev of
                        (_, absEqs) -> if prev == absEqs then absEqs
                                         else loop absEqs
      in loop Map.empty

reduceEqs = liftM eqUnions . mapM reduceEq
reduceEq Base      = return [Base]
reduceEq (Heap hp) = lookupEq (HeapPointer hp) -- return [Heap hp]
reduceEq (Ident i) = lookupEq (Equation i)
reduceEq (Extract eqs tag n)
    = do eqs' <- reduceEqs eqs
         reduceEqs (eqUnions [ args !! n | Tag t args <- eqs', t == tag ])
-- FIXME: Perform sharing analysis to determine which HeapPointers may be updated.
--        We can discard the (t++"_result") data if the variable isn't shared.
reduceEq (Tag t args) | tagIsSuspFunction t
    = do result <- reduceEq (Ident (atomToVar (tagFlipFunction t) "_result"))
         args' <- mapM reduceEqs args
         return $ eqUnion [Tag t args] result
reduceEq (Tag t args)
    = do args' <- mapM reduceEqs args
         return [Tag t args]
reduceEq (Eval i)
    = do vals <- lookupEq (Equation i)
         let f (Tag tag args) = if tagIsSuspFunction tag
                                   then reduceEq (Ident (atomToVar (tagFlipFunction tag) "_result"))
                                   else return [Tag tag args]
             f (Heap hp) = lookupEq (HeapPointer hp)
             -- FIXME: It should be impossible to encounter a 'Base' here.
             f Base = return []
             f t = error $ "reduceEq: eval: " ++ show (t,i,vals)
         liftM eqUnions $ mapM f vals
reduceEq (Apply a b)
    = do vals <- lookupEq (Equation a)
         let f (Tag tag args) = case tagUnfunction tag of
                                  Just (0,func) -> error $ "Apply to fully applied function. " ++ show (a,b,vals)
                                  Just (1,func) -> reduceEq (Ident (atomToVar func "_result"))
                                  Just (n,func) -> return [Tag (partialTag func (n-1)) (args ++ [[Ident b]])]
                                  Nothing       -> error "Apply to data constructor"
             f t = error $ "reduceEq: apply: " ++ show t
         liftM eqUnions $ mapM f vals
reduceEq (PartialApply a b)
    = do vals <- lookupEq (Equation a)
         let f (Tag tag args) = case tagUnfunction tag of
                                  Just (0,func) -> error $ "Apply to fully applied function. " ++ show (a,b,vals)
                                  Just (n,func) -> return [Tag (partialTag func (n-1)) (args ++ [[Ident b]])]
                                  Nothing       -> error "Apply to data constructor"
             f t = error $ "reduceEq: apply: " ++ show t
         liftM eqUnions $ mapM f vals
reduceEq e =  error $ "Unhandled input: " ++ show e

lookupEq :: EqPointer -> Simplify [Equation]
lookupEq v
  = Simplify $ \eqs -> (Map.findWithDefault [] v eqs, eqs)

updateEq :: EqPointer -> [Equation] -> Simplify ()
updateEq v eq
  = Simplify $ \eqs -> ((), Map.insert v eq eqs)
