{-# LANGUAGE CPP #-}
module Grin.Lint(
    lintCheckGrin,
    typecheckGrin,
    transformGrin,
    dumpGrin
    ) where

import Control.Exception.Extensible as Ex
import Control.Monad.Reader
import Data.Monoid
import System.IO
import qualified Data.Set as Set

import Doc.DocLike
import Grin.Grin
import Grin.Show
import Options
import Support.FreeVars
import Support.Transform
import Util.Gen
import Util.SetLike
import qualified FlagDump as FD
import qualified Stats


lintCheckGrin grin = when flint $ typecheckGrin grin

lintCheckGrin' onerr grin | flint = do
    let env = TcEnv { envTyEnv = grinTypeEnv grin, envInScope = fromList (fsts $ grinCafs grin) }
    let errs = [  (err ++ "\n" ++ render (prettyFun a) ) | (a,Left err) <-  [ (a,runTc env (tcLam Nothing c))  | a@(_,c) <-  grinFuncs grin ]]
    if null errs then return () else do
    onerr
    putErrLn ">>> Type Errors"
    mapM_ putErrLn  errs
    unless (null errs || optKeepGoing options) $ fail "There were type errors!"
lintCheckGrin' _ _ = return ()

typecheckGrin grin = do
    let env = TcEnv { envTyEnv = grinTypeEnv grin, envInScope = fromList (fsts $ grinCafs grin) }
    let errs = [  (err ++ "\n" ++ render (prettyFun a) ) | (a,Left err) <-  [ (a,runTc env (tcLam Nothing c))  | a@(_,c) <-  grinFuncs grin ]]
    mapM_ putErrLn  errs
    unless (null errs || optKeepGoing options) $ fail "There were type errors!"

dumpGrin pname grin = do
    let fn = optOutName options ++ "_" ++ pname ++ ".grin"
    --putErrLn $ "Writing: " ++ fn
    h <- openFile fn  WriteMode
    (argstring,sversion) <- getArgString
    hPutStrLn h $ unlines [ "-- " ++ argstring,"-- " ++ sversion,""]
    hPrintGrin h grin
    hClose h
    {-wdump FD.Grin $ do
        putErrLn $ "v-- " ++ pname ++ " Grin"
        printGrin grin
        putErrLn $ "^-- " ++ pname ++ " Grin"-}


transformGrin :: TransformParms Grin -> Grin -> IO Grin

transformGrin TransformParms { transformIterate = IterateMax n } prog | n <= 0 = return prog
transformGrin TransformParms { transformIterate = IterateExactly n } prog | n <= 0 = return prog
transformGrin tp prog = do
    let dodump = transformDumpProgress tp
        name = transformCategory tp ++ pname (transformPass tp) ++ pname (transformName tp)
        _scname = transformCategory tp ++ pname (transformPass tp)
        pname "" = ""
        pname xs = '-':xs
        iterate = transformIterate tp
    when dodump $ putErrLn $ "-- " ++ name
    let ferr e = do
        putErrLn $ "\n>>> Exception thrown"
        putErrLn $ "\n>>> Before " ++ name
        dumpGrin ("lint-before-" ++ name) prog
        putErrLn $ "\n>>>"
        putErrLn (show (e::SomeException))
        maybeDie
        return prog
    let istat = grinStats prog
    prog' <- Ex.catch (transformOperation tp prog { grinStats = mempty } >>= Ex.evaluate ) ferr
    let estat = grinStats prog'
    let onerr grin' = do
            putErrLn $ "\n>>> Before " ++ name
            dumpGrin ("lint-before-" ++ name) prog
            Stats.printStat name estat
            putErrLn $ "\n>>> After " ++ name
            dumpGrin ("lint-after-" ++ name) grin'
    if transformSkipNoStats tp && Stats.null estat then do
        when dodump $ putErrLn "program not changed"
        return prog
     else do
    when (dodump && not (Stats.null estat)) $ Stats.printStat  name estat
    lintCheckGrin' (onerr prog') prog'
    let tstat = istat `mappend` estat
    if doIterate iterate (not $ Stats.null estat) then transformGrin tp { transformIterate = iterateStep iterate } prog' { grinStats = tstat } else return prog' { grinStats = tstat }
--    if doIterate iterate (estat /= mempty) then transformGrin tp { transformIterate = iterateStep iterate } prog' { progStats = istat `mappend` estat } else
--        return prog' { progStats = istat `mappend` estat, progPasses = name:progPasses prog' }


maybeDie = case optKeepGoing options of
    True -> return ()
    False -> putErrDie "Internal Error"


data TcEnv = TcEnv {
    envTyEnv :: TyEnv,
    envInScope :: Set.Set Var
}



newtype Tc a = Tc (ReaderT TcEnv (Either String) a)
    deriving(Monad,MonadReader TcEnv)


runTc :: TcEnv -> Tc a -> Either String a
runTc env (Tc r) = runReaderT r env

same _ t1 t2 | t1 == t2 = return t1
same msg t1 t2 = fail $ "Types not the same:" <+> parens msg <+> parens (tshow t1) <+> parens (tshow t2)

tcLam :: Maybe [Ty] -> Lam -> Tc [Ty]
tcLam mty (v :-> e) = f mty where
    f Nothing = ans (mapM tcVal v)
    f (Just ty) = ans $ do
        t <- mapM tcVal v
        same (":->" <+> show mty <+> show (v :-> e)) ty t
    ans r = local (\e -> e { envInScope = freeVars v `mappend` envInScope e }) $ r >> tcExp e

tcExp :: Exp -> Tc [Ty]
tcExp e = f e where
    f (e :>>= lam) = do
        t1 <- f e
        tcLam (Just t1) lam
    f n@(Prim p as t') = do
        mapM_ tcVal as
        return t'
    f ap@(App fn [v,a] t) | fn == funcApply = do
        [v',a'] <- mapM tcVal [v,a]
        if v' == TyNode then return t
         else fail $ "App apply arg doesn't match: " ++ show ap
    f ap@(App fn [v] t) | fn == funcApply = do
        v' <- tcVal v
        if v' == TyNode then return t
         else fail $ "App apply arg doesn't match: " ++ show ap
    f ap@(App fn [v] t) | fn == funcEval = do
        v' <- tcVal v
        if v' == tyINode then return t
         else fail $ "App eval arg doesn't match: " ++ show ap
    f a@(App fn as t) = do
        te <- asks envTyEnv
        (as',t') <- findArgsType te fn
        as'' <- mapM tcVal as
        if t' == t then
            if as'' == as' then return t' else
                fail $ "App: arguments do not match: " ++ show (a,as',t')
         else fail $ "App: results do not match: " ++ show (a,t,(as',t'))
    f (Store v) = do
        t <- tcVal v
        return [TyPtr t]
    f Alloc { expValue = v } = do
        t <- tcVal v
        return [TyPtr t]
    f (Return v) = mapM tcVal v
    f (Fetch v) = do
        (TyPtr t) <- tcVal v
        return [t]
    f (Error _ t) = return t
    f e@(Update w v) = do
        (TyPtr t) <- tcVal w
        t' <- tcVal v
        same (show e) t t'
        return []
    f (Case _ []) = fail "empty case"
    f (Case v as) = do
        tv <- tcVal v
        es <- mapM (tcLam (Just [tv])) as
        foldl1M (same $ "case exp: " ++ show (map head $ sortGroupUnder fst (zip es as)) ) es
{-    f (Let { expDefs = defs, expBody = body }) = do
        local (\e -> e { envTyEnv = extendTyEnv defs (envTyEnv e) }) $ do
            mapM_ (tcLam Nothing) [ b | FuncDef { funcDefBody = b } <- defs ]
            f body-}

tcVal :: Val -> Tc Ty
tcVal v = f v where
    f e@(Var v t) = do
        s <- asks envInScope
        case v `member` s of
            True -> return t
            False -> fail $ "variable not in scope: " ++ show e
    f (Lit _ t) = return t
    f Unit = return TyUnit
    f (Const t) = do
        v <- f t
        return (TyPtr v)
    f (Index v offset) = do
        t <- f v
        TyPrim _ <- f offset
        return t
    f (ValUnknown ty) = return ty
    f (ValPrim _ vs ty) = do mapM_ f vs >> return ty
    f n@(NodeC tg as) = do
        te <- asks envTyEnv
        (as',_) <- findArgsType te tg
        as'' <- mapM f as
        if as'' == as' then return TyNode else
            fail $ "NodeC: arguments do not match " ++ show n ++ show (as'',as')
    f (Item _ t) = return t


