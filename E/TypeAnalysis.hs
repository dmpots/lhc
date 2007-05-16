-- | examine all uses of types in a program to determine which ones are
-- actually needed in the method generation

module E.TypeAnalysis(typeAnalyze, Typ(),expandPlaceholder) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Identity
import Data.Monoid
import Data.FunctorM
import qualified Data.Set as Set
import qualified Data.Map as Map

import DataConstructors
import Doc.PPrint
import E.Annotate
import E.E hiding(isBottom)
import E.Eta
import E.Program
import E.Rules
import E.Subst
import E.Traverse(emapE',emapE_,emapE)
import E.TypeCheck
import E.Values
import Fixer.Fixer
import Fixer.Supply
import Fixer.VMap
import GenUtil
import Info.Info(infoMapM,infoMap)
import Info.Types
import Name.Id
import Name.Name
import Name.Names
import Stats
import Support.CanType
import Support.FreeVars
import Util.Gen
import Util.SetLike
import qualified Info.Info as Info


type Typ = VMap () Name
data Env = Env {
    envRuleSupply :: Supply (Module,Int) Bool,
    envValSupply :: Supply TVr Bool,
    envEnv :: IdMap [Value Typ]
    }

extractValMap :: [(TVr,E)] -> IdMap [Value Typ]
extractValMap ds = fromList [ (tvrIdent t,f e []) | (t,e) <- ds] where
    f (ELam tvr e) rs | sortKindLike (getType tvr) = f e (runIdentity (Info.lookup $ tvrInfo tvr):rs)
    f _ rs = reverse rs

-- all variables _must_ be unique before running this
{-# NOINLINE typeAnalyze #-}
typeAnalyze :: Bool -> Program -> IO Program
typeAnalyze doSpecialize prog = do
    fixer <- newFixer
    ur <- newSupply fixer
    uv <- newSupply fixer
    let lambind _ nfo = do
            x <- newValue fixer ( bottom :: Typ)
            return $ Info.insert x (Info.delete (undefined :: Typ) nfo)
        lamread _ nfo | Just v <- Info.lookup nfo = do
            rv <- readValue v
            return (Info.insert (rv :: Typ) $ Info.delete (undefined :: Value Typ) nfo)
        lamread _ nfo = return nfo
        lamdel _ nfo = return (Info.delete (undefined :: Value Typ) nfo)
    prog <- annotateProgram mempty lambind (\_ -> return . deleteArity) (\_ -> return) prog
    let ds = programDs prog
        env = Env { envRuleSupply = ur, envValSupply = uv, envEnv = extractValMap ds }
        entries = progEntryPoints prog
    calcDs env ds
    forM_ entries $ \tvr ->  do
        vv <- supplyValue uv tvr
        addRule $ assert vv
    mapM_ (sillyEntry env) entries
    findFixpoint Nothing fixer
    prog <- annotateProgram mempty (\_ -> return) (\_ -> return) lamread prog
    unusedRules <- supplyReadValues ur >>= return . fsts . filter (not . snd)
    unusedValues <- supplyReadValues uv >>= return . fsts . filter (not . snd)
    let (prog',stats) = runStatM $ specializeProgram doSpecialize (Set.fromList unusedRules) (Set.fromList unusedValues) prog
    prog <- annotateProgram mempty lamdel (\_ -> return) (\_ -> return) prog'
    return prog { progStats = progStats prog `mappend` stats }

sillyEntry :: Env -> TVr -> IO ()
sillyEntry env t = mapM_ (addRule . (`isSuperSetOf` value (vmapPlaceholder ()))) args where
    args = lookupArgs t env

lookupArgs t Env { envEnv = tm }  = maybe [] id (mlookup (tvrIdent t) tm)

toLit (EPi TVr { tvrType = a } b) = return (tc_Arrow,[a,b])
toLit (ELit LitCons { litName = n, litArgs = ts }) = return (n,ts)
toLit _ = fail "not convertable to literal"

assert :: Value Bool -> Fixer.Fixer.Rule
assert v = v `isSuperSetOf` value True

calcDef :: Env -> (TVr,E) -> IO ()
calcDef env@Env { envRuleSupply = ur, envValSupply = uv } (t,e) = do
    let (_,ls) = fromLam e
        tls = takeWhile (sortKindLike . getType) ls
        rs = rulesFromARules (Info.fetch (tvrInfo t))
        hr r = do
            ruleUsed <- supplyValue ur (ruleUniq r)
            addRule $ conditionalRule id ruleUsed (ioToRule $  calcE env (ruleBody r))
            let hrg r (t,EVar a) | a `elem` ruleBinds r = do
                    let (t'::Value Typ) = Info.fetch (tvrInfo t)
                    let (a'::Value Typ) = Info.fetch (tvrInfo a)
                    addRule $ conditionalRule id ruleUsed $ ioToRule $ do
                        addRule $ a' `isSuperSetOf` t'
                    return True
                hrg r (t,e) | Just (n,as) <- toLit e = do
                    let (t'::Value Typ) = Info.fetch (tvrInfo t)
                    as' <- mapM getValue as
                    addRule $ conditionalRule id ruleUsed $ ioToRule $ do
                        forMn_ ((zip as as')) $ \ ((a',a''),i) -> do
                            when (isEVar a') $ addRule $ modifiedSuperSetOf a'' t' (vmapArg n i)
                            --addRule $ modifiedSuperSetOf t' vv (vmapArg n i)
--                    addRule $ conditionalRule id ruleUsed $ ioToRule $ do
--                        flip mapM_ (zip as' naturals)  $ \ (v,i) -> do
--                            addRule $ modifiedSuperSetOf vv v (vmapArgSingleton n i)
                    --addRule $ dynamicRule v $ \v' -> (mconcat [ t `isSuperSetOf` value (vmapArg n i v') | (t,i) <-  zip ts' naturals ])
                    addRule $ conditionalRule (n `vmapMember`) t' (assert ruleUsed)
                    return False
            rr <- mapM (hrg r) (zip tls (ruleArgs r))
            when (and rr) $ addRule (assert ruleUsed)
    valUsed <- supplyValue uv t
    addRule $ conditionalRule id valUsed $ ioToRule $ do
        mapM_ hr rs
        calcE env e

calcDs ::  Env -> [(TVr,E)] -> IO ()
calcDs env@Env { envRuleSupply = ur, envValSupply = uv } ds = do
    mapM_ d ds
    forM_ ds $ \ (v,e) -> do calcDef env (v,e)
      --  addRule $ conditionalRule id nv (ioToRule $ calcDef env (v,e))
     where
    d (t,e) | not (sortKindLike (getType t)) = return ()
    d (t,e) | Just v <- getValue e = do
        let Just t' = Info.lookup (tvrInfo t)
        addRule $ t' `isSuperSetOf` v
    d (t,e) | Just (n,xs) <- toLit e = do
        let Just t' = Info.lookup (tvrInfo t)
            v = vmapSingleton n
        addRule $ t' `isSuperSetOf` (value v)
        xs' <- mapM getValue xs
        forMn_ xs' $ \ (v,i) -> do
            addRule $ modifiedSuperSetOf t' v (vmapArgSingleton n i)
    d (t,e) | (EVar v,as) <- fromAp e = do
        let Just t' = Info.lookup (tvrInfo t)
            Just v' = Info.lookup (tvrInfo v)
        as' <- mapM getValue as
        addRule $ dynamicRule v' $ \ v -> mconcat $ (t' `isSuperSetOf` value (vmapDropArgs v)):case vmapHeads v of
                Just vs -> concat $ flip map vs $ \h -> (flip map (zip as' [0.. ])  $ \ (a,i) -> modifiedSuperSetOf t' a $ \ v -> vmapArgSingleton h i v)
                Nothing -> flip map (zip as' [0.. ])  $ \ (_,i) -> isSuperSetOf t' (value $ vmapProxyIndirect i v)
    d (t,e) = fail $ "calcDs: " ++ show (t,e)

-- TODO - make default case conditional
calcAlt env v (Alt LitCons { litName = n, litArgs = xs } e) = do
    addRule $ conditionalRule (n `vmapMember`) v $ ioToRule $ do
        calcE env e
        forMn_ xs $ \ (t,i) -> do
            let Just t' = Info.lookup (tvrInfo t)
            addRule $ modifiedSuperSetOf t' v (vmapArg n i)


calcE :: Env -> E -> IO ()
calcE env (ELetRec ds e) = calcDs nenv ds >> calcE nenv e where
    nenv = env { envEnv = extractValMap ds `union` envEnv env }
calcE env e | (e',(_:_)) <- fromLam e = calcE env e'
calcE env ec@ECase {} | sortKindLike (getType $ eCaseScrutinee ec) = do
    calcE env (eCaseScrutinee ec)
    fmapM_ (calcE env) (eCaseDefault ec)
    v <- getValue (eCaseScrutinee ec)
    mapM_ (calcAlt env v) (eCaseAlts ec)
calcE env ec@ECase {} = do
    calcE env (eCaseScrutinee ec)
    mapM_ (calcE env) (caseBodies ec)
calcE env e@ELit {} = tagE env e
calcE env e@EPrim {} = tagE env e
calcE _ EError {} = return ()
calcE _ ESort {} = return ()
calcE _ Unknown = return ()
calcE env e | (EVar v,as@(_:_)) <- fromAp e = do
    let ts = lookupArgs v env
    tagE env e
    when (length as < length ts) $ fail ("calcE: unsaturated call to function: " ++ pprint e)
    forM_ (zip as ts) $ \ (a,t) -> do
        when (sortKindLike (getType a)) $ do
            a' <- getValue a
            addRule $ t `isSuperSetOf` a'
calcE env e@EVar {} = tagE env e
calcE env e@EAp {} = tagE env e
calcE env e@EPi {} = tagE env e
calcE _ e = fail $ "odd calcE: " ++ show e

tagE Env { envValSupply = uv }  (EVar v) | not $ getProperty prop_RULEBINDER v = do
    v <- supplyValue uv v
    addRule $ assert v
tagE env e  = emapE_ (tagE env) e

getValue (EVar v)
    | Just x <- Info.lookup (tvrInfo v) = return x
    | otherwise = return $ value (vmapPlaceholder ())
    -- | otherwise = fail $ "getValue: no varinfo: " ++ show v
getValue e | Just c <- typConstant e = return $ value c
getValue e = value `liftM` fuzzyConstant e  -- TODO - make more accurate
getValue e = fail $ "getValue: " ++ show e

fuzzyConstant e | Just c <- typConstant e = return c
fuzzyConstant e | Just (n,as) <- toLit e = do
    as' <- mapM fuzzyConstant as
    return $ vmapValue n as'
fuzzyConstant _ = return $ (vmapPlaceholder ())


typConstant :: Monad m => E -> m Typ
typConstant (EPi TVr { tvrType = a} b) = do
    ab <- mapM typConstant [a,b]
    return $ vmapValue tc_Arrow ab
typConstant (ELit LitCons { litName = n, litArgs = xs }) = do
    xs' <- mapM typConstant xs
    return $ vmapValue n xs'
typConstant e = fail $ "typConstant: " ++ show e


-- pruning the unused branches of typecase statements


pruneE :: E -> IO E
pruneE e = return $ runIdentity (prune e)  where
    prune ec@ECase { eCaseScrutinee = EVar v } | sortKindLike (getType v), Just vm <- Info.lookup (tvrInfo v) = do
        ec' <- pruneCase ec vm
        emapE' prune ec'
    prune e = emapE' prune e

pruneCase :: (Monad m) => E -> VMap () Name -> m E
pruneCase ec ns = return $ if null (caseBodies nec) then err else nec where
    err = EError "pruneCase: all alternatives pruned" (getType ec)
    nec = caseUpdate ec { eCaseAlts = f [] $ eCaseAlts ec, eCaseDefault = cd (eCaseDefault ec)}
    f xs [] = reverse xs
    f xs (alt@(Alt LitCons { litName = n } _):rs) | not (n `vmapMember` ns) = f xs rs
    f xs (alt:rs) = f (alt:xs) rs
    cd (Just d) | Just nns <- vmapHeads ns, or [ n `notElem` as | n <- nns ] = Just d
                | Nothing <- vmapHeads ns = Just d
    cd Nothing = Nothing
    -- The reason we do this is because for a typecase, we need a valid default in order to get the most general type
    cd (Just d) = Just $ EError "pruneCase: default pruned" (getType d)
    as = [ n | LitCons { litName = n } <- casePats ec ]


data SpecEnv = SpecEnv {
    senvUnusedRules :: Set.Set (Module,Int),
    senvUnusedVars  :: Set.Set TVr,
    senvDataTable :: DataTable,
    senvArgs      :: Map.Map TVr [Int]
    }



getTyp :: Monad m => E -> DataTable -> Typ -> m E
getTyp kind dataTable vm = f 10 kind vm where
    f n _ _ | n <= 0 = fail "getTyp: too deep"
    -- f n kind vm | Just [] <- vmapHeads vm = return $ tAbsurd kind  TODO - absurdize values properly
    f n kind vm | Just [h] <- vmapHeads vm = do
        let ss = slotTypes dataTable h kind
            as = [ (s,vmapArg h i vm) | (s,i) <- zip ss [0..]]
        as'@(~[fa,fb]) <- mapM (uncurry (f (n - 1))) as
        if h == tc_Arrow
         then return $ EPi tvr { tvrType = fa } fb
         else return $ ELit (updateLit dataTable litCons { litName = h, litArgs = as', litType = kind })
    f _ _ _  = fail "getTyp: not constant type"

specializeProgram :: (MonadStats m) =>
    Bool                       -- ^ do specialization
    -> (Set.Set (Module,Int))  -- ^ unused rules
    -> (Set.Set TVr)           -- ^ unused values
    -> Program
    -> m Program
specializeProgram doSpecialize unusedRules unusedValues prog = do
    (nds,_) <- specializeDs doSpecialize SpecEnv { senvUnusedRules = unusedRules, senvUnusedVars = unusedValues, senvDataTable = progDataTable prog, senvArgs = mempty } (programDs prog)
    return $ programSetDs nds prog


repi (ELit LitCons { litName = n, litArgs = [a,b] }) | n == tc_Arrow = EPi tvr { tvrIdent = 0, tvrType = repi a } (repi b)
repi e = runIdentity $ emapE (return . repi ) e

specializeDef _ SpecEnv { senvUnusedVars = unusedVals }  (tvr,e) | tvr `Set.member` unusedVals = return (tvr,EError "Unused" (tvrType tvr))
specializeDef _ _ (t,e) | getProperty prop_PLACEHOLDER t = return (t,e)
specializeDef True SpecEnv { senvDataTable = dataTable }  (tvr,e) = ans where
    sub = substMap''  $ fromList [ (tvrIdent t,Just v) | (t,Just v) <- sts ]
    sts = map spec ts
    spec t | Just nt <- Info.lookup (tvrInfo t) >>= getTyp (getType t) dataTable, sortKindLike (getType t) = (t,Just (repi nt))
    spec t = (t,Nothing)
    (fe,ts) = fromLam e
    ne = sub $ foldr ELam fe [ t | (t,Nothing) <- sts]
    ans = do
        sequence_ [ mtick ("Specialize.body.{" ++ pprint tvr ++ "}.{" ++ pprint t ++ "}.{" ++ pprint v) | (t,Just v) <- sts ]
        let vs = [ (n,v) | ((_,Just v),n) <- zip sts naturals ]
            sd = not $ null vs
        when sd $ tell (msingleton tvr (fsts vs))
        return (if sd then tvr { tvrType = infertype dataTable ne, tvrInfo = infoMap (dropArguments vs) (tvrInfo tvr) } else tvr,ne)
specializeDef _ _ (t,e) = return (t,e)


specBody :: MonadStats m => Bool -> SpecEnv -> E -> m E
specBody _ env@SpecEnv { senvUnusedVars = unusedVars, senvDataTable = dataTable } e | (EVar h,as) <- fromAp e, h `Set.member` unusedVars = do
    mtick $ "Specialize.delete.{" ++ pprint h ++ "}"
    return $ foldl EAp (EError ("Unused: " ++ pprint h) (getType h)) as
specBody True SpecEnv { senvArgs = dmap } e | (EVar h,as) <- fromAp e, Just os <- mlookup h dmap = do
    mtick $ "Specialize.use.{" ++ pprint h ++ "}"
    return $ foldl EAp (EVar h) [ a | (a,i) <- zip as naturals, i `notElem` os ]
--specBody True env e@ECase { eCaseScrutinee = EVar v } | sortKindLike (getType v)scrut = do
--    k
specBody doSpecialize env (ELetRec ds e) = do
    (nds,nenv) <- specializeDs doSpecialize env ds
    e <- specBody doSpecialize nenv e
    return $ ELetRec nds e
specBody doSpecialize env e = emapE' (specBody doSpecialize env) e

--specializeDs :: MonadStats m => DataTable -> Map.Map TVr [Int] -> [(TVr,E)] -> m ([(TVr,E)]
specializeDs doSpecialize env@SpecEnv { senvUnusedRules = unusedRules, senvDataTable = dataTable }  ds = do
    (ds,nenv) <- runWriterT $ mapM (specializeDef doSpecialize env) ds
    -- ds <- sequence [ specBody dataTable (nenv `mappend` env) e >>= return . (,) t | (t,e) <- ds]
    let f (t,e) = do
            e <- sb e
            nfo <- infoMapM (mapABodiesArgs sb) (tvrInfo t)
            nfo <- infoMapM (return . arules . filter ( not . (`Set.member` unusedRules) . ruleUniq) . rulesFromARules) nfo
            return (t { tvrInfo = nfo }, e)
        tenv = env { senvArgs = nenv `mappend` senvArgs env }
        sb = specBody doSpecialize tenv
    ds <- mapM f ds
    return (ds,tenv)


expandPlaceholder :: Monad m => (TVr,E) -> m (TVr,E)
expandPlaceholder (tvr,oe) | getProperty prop_PLACEHOLDER tvr = do
    let rules = filter isBodyRule $  rulesFromARules $ Info.fetch (tvrInfo tvr)
        isBodyRule Rule { ruleBody = e } | (EVar vv,_) <- fromAp e, getProperty prop_INSTANCE vv = True
        isBodyRule _ = False
    if null rules then return (unsetProperty prop_PLACEHOLDER tvr, EError ("Placeholder, no bodies: " ++ tvrShowName tvr) (getType tvr)) else do
    let (oe',as) = fromLam oe
        rule1:_ = rules
        ct = getType $ foldr ELam oe' (drop (length $ ruleArgs rule1) as)
        as'@(a:ras)
                | (a:ras) <- take (length $ ruleArgs rule1) as = (a:ras)
                | otherwise = error $ pprint (tvr,(oe,show rule1))
        ne = emptyCase {
            eCaseScrutinee = EVar a,
            eCaseAlts = map calt rules,
            eCaseBind = a { tvrIdent = 0 },
            eCaseType = ct
            }
        calt rule@Rule { ruleArgs = (arg:rs) } = Alt vp (substMap (fromList [ (tvrIdent v,EVar r) | ~(EVar v) <- rs | r <- ras ]) $ ruleBody rule) where
            Just vp = eToPat arg
    return (unsetProperty prop_PLACEHOLDER tvr,foldr ELam ne as')

expandPlaceholder _x = fail "not placeholder"









