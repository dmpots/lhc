{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, NoMonomorphismRestriction #-}
module Grin.Stage2.Optimize.Simple
    ( optimize
    ) where

import Grin.Stage2.Types

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map as Map

type Opt a = Reader Subst a
type Subst = Map.Map Renamed Renamed


optimize :: Grin -> Grin
optimize = grinTrivialCaseCase . grinSimple

grinSimple :: Grin -> Grin
grinSimple grin
    = grin{ grinFunctions = map simpleFuncDef (grinFunctions grin)}


simpleFuncDef :: FuncDef -> FuncDef
simpleFuncDef def
    = def{ funcDefBody = runSimpleExpression (funcDefBody def) }

runSimpleExpression :: Expression -> Expression
runSimpleExpression e = runReader (simpleExpression e) Map.empty

simpleExpression :: Expression -> Opt Expression
simpleExpression (Unit v1 :>>= v2 :-> b)
    = do v1' <- doSubsts v1
         subst (zip v2 (v1' ++ repeat (Builtin "undefined"))) (simpleExpression b)
simpleExpression (a :>>= v1 :-> Unit v2) | v1 == v2
    = simpleExpression a
simpleExpression ((a :>>= b :-> c) :>>= d)
    = simpleExpression (a :>>= b :-> c :>>= d)
simpleExpression (a :>>= b :-> c)
    = do a' <- simpleExpression a
         c' <- simpleExpression c
         return (a' :>>= b :-> c')
simpleExpression (Application fn values)
    = liftM (Application fn) $ doSubsts values
simpleExpression (StoreHole size)
    = return $ StoreHole size
simpleExpression (Store vs)
    = liftM Store $ mapM doSubst vs
simpleExpression (Unit values)
    = liftM Unit (mapM doSubst values)
--simpleExpression (Case var [_ :> alt])
--    = simpleExpression alt
simpleExpression (Case val alts)
    = do val' <- doSubst val
         alts' <- mapM simpleAlt alts
         return $ Case val' alts'
simpleExpression (Fetch n p)
    = liftM (Fetch n) (doSubst p)
simpleExpression (Constant c)
    = return $ Constant c


simpleAlt :: Alt -> Opt Alt
simpleAlt (v :> e) = do e' <- simpleExpression e
                        return (v :> e')


doSubst var
    = asks $ \m -> case Map.lookup var m of
                     Nothing     -> var
                     Just newVar -> newVar

--doSubsts :: [Renamed] -> Opt [Renamed]
doSubsts = mapM doSubst

--subst :: [(Renamed, Renamed)] -> Opt a -> Opt a
subst pairs = local $ \m -> Map.fromList pairs `Map.union` m






type M a = ReaderT Subst (State Int) a

grinTrivialCaseCase :: Grin -> Grin
grinTrivialCaseCase grin
    = case runState (runReaderT action Map.empty) (grinUnique grin) of
        (grin, newUnique) -> grin{grinUnique = newUnique}
    where action = do defs <- mapM trivialCaseFuncDef (grinFunctions grin)
                      return grin{grinFunctions = defs}

trivialCaseFuncDef :: FuncDef -> M FuncDef
trivialCaseFuncDef def
    = do body <- trivialCaseCase (funcDefBody def)
         return def{ funcDefBody = body }


isTrivialExpression :: Expression -> Bool
isTrivialExpression Unit{} = True
isTrivialExpression Application{} = True
isTrivialExpression _ = False

{-
  [n] <- case a of
          A -> ...
          B -> ...
          C -> ...
  [i] <- case a of
          A -> ...
          B -> ...
          C -> ...
=====>
  [n,i] <- case a of
            A -> ...
            B -> ...
            C -> ...
-}
trivialCaseCase :: Expression -> M Expression
trivialCaseCase (Case scrut1 alts1 :>>= binds1 :-> Case scrut2 alts2 :>>= binds2 :-> e) | scrut1 == scrut2
    = do alts <- mapM (joinAlt binds1 binds2 alts2) alts1
         trivialCaseCase (Case scrut1 alts :>>= (binds1++binds2) :-> e)
trivialCaseCase (Case scrut alts :>>= binds :-> e) | isTrivialExpression e
    = do alts' <- forM alts $ \(cond :> branch) -> do binds' <- replicateM (length binds) newVariable
                                                      e' <- subst (zip binds binds') (renameExp e)
                                                      return (cond :> (branch :>>= binds' :-> e'))
         trivialCaseCase (Case scrut alts')
trivialCaseCase (Application fn values)
    = liftM (Application fn) $ doSubsts values
trivialCaseCase (StoreHole size)
    = return $ StoreHole size
trivialCaseCase (Store vs)
    = liftM Store $ mapM doSubst vs
trivialCaseCase (Unit values)
    = liftM Unit (mapM doSubst values)
trivialCaseCase (Case val alts)
    = do val' <- doSubst val
         alts' <- mapM trivialCaseAlt alts
         return $ Case val' alts'
trivialCaseCase (Fetch n p)
    = liftM (Fetch n) (doSubst p)
trivialCaseCase (Constant c)
    = return $ Constant c
trivialCaseCase (e1 :>>= binds :-> e2)
    = do e1' <- trivialCaseCase e1
         e2' <- trivialCaseCase e2
         return $ e1' :>>= binds :-> e2'

trivialCaseAlt :: Alt -> M Alt
trivialCaseAlt (v :> e) = do e' <- trivialCaseCase e
                             return (v :> e')


renameExp :: Expression -> M Expression
renameExp (Application fn values)
    = liftM (Application fn) $ doSubsts values
renameExp (StoreHole size)
    = return $ StoreHole size
renameExp (Store vs)
    = liftM Store $ mapM doSubst vs
renameExp (Unit values)
    = liftM Unit (mapM doSubst values)
renameExp (Case val alts)
    = do val' <- doSubst val
         alts' <- mapM renameAlt alts
         return $ Case val' alts'
renameExp (Fetch n p)
    = liftM (Fetch n) (doSubst p)
renameExp (Constant c)
    = return $ Constant c
renameExp (e1 :>>= binds :-> e2)
    = do e1' <- trivialCaseCase e1
         binds' <- replicateM (length binds) newVariable
         e2' <- subst (zip binds binds') (trivialCaseCase e2)
         return $ e1' :>>= binds' :-> e2'


renameAlt :: Alt -> M Alt
renameAlt (v :> e) = do e' <- renameExp e
                        return (v :> e')

{-
A -> m a
=====>
A -> do [n] <- m a
        [i] <- m b
        unit [n,i]
-}
joinAlt binds1 binds2 branches (cond :> branch)
    = do binds1' <- replicateM (length binds1) newVariable
         binds2' <- replicateM (length binds2) newVariable
         let newBranch = findBranch branches
         branch' <- return branch
         exp' <- subst (zip binds1 binds1') (renameExp newBranch)
         return (cond :> (branch' :>>= binds1' :-> exp' :>>= binds2' :-> Unit (binds1'++binds2')))
    where findBranch [] = Application (Builtin "unreachable") []
          findBranch ((c :> branch):xs) | c == cond = branch
                                        | otherwise = findBranch xs

newVariable :: M Renamed
newVariable
    = do uid <- newUnique
         return $ Anonymous uid

newUnique :: M Int
newUnique
    = do uid <- get
         put (uid+1)
         return uid

