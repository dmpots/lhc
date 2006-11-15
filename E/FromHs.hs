module E.FromHs(
    convertDecls,
    convertRules,
    createInstanceRules,
    makeSpec,
    getMainFunction
    ) where

import Char
import Control.Monad.Identity
import Control.Monad.RWS
import Data.FunctorM
import Data.Monoid
import List(isPrefixOf)
import Maybe
import Prelude hiding((&&),(||),not,and,or,any,all,head)
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PPrint

import Atom
import Boolean.Algebra
import C.FFI
import C.Prims as CP
import FrontEnd.Class
import DataConstructors
import Doc.DocLike
import Doc.PPrint
import E.E
import E.Eta
import E.Eval(eval)
import E.LetFloat(atomizeAp)
import E.PrimOpt
import E.Rules
import E.Subst
import E.Traverse
import E.TypeAnalysis
import E.TypeCheck
import E.Values
import Fixer.VMap
import FrontEnd.Rename(unRename)
import FrontEnd.SrcLoc
import FrontEnd.Tc.Module(TiData(..))
import FrontEnd.Tc.Type hiding(Rule(..))
import FrontEnd.Utils
import HsSyn as HS
import Info.Types
import Name.Name as Name
import Name.Names
import Name.VConsts
import Options
import PrimitiveOperators
import Support.CanType
import Support.FreeVars
import Util.Gen
import Util.NameMonad
import Util.SetLike
import qualified FlagOpts as FO
import qualified FrontEnd.Tc.Monad as TM
import qualified FrontEnd.Tc.Type as T(Rule(..))
import qualified Info.Info as Info
import qualified Stats

ump sl e = EError (show sl ++ ": Unmatched pattern") e


createIf e a b = do
    [tv] <- newVars [Unknown]
    return $ createIfv tv e a b

createIfv v e a b = res where
    tv = v { tvrType = tIntzh }
    ic = eCase (EVar tv) [Alt (LitInt 1 tIntzh) a, Alt (LitInt 0 tIntzh) b] Unknown
    res = eCase e [Alt (litCons { litName = dc_Boolzh, litArgs = [tv], litType = tBool }) ic] Unknown


head (x:_) = x
head _ = error "FromHsHeadError"

--newVars :: MonadState Int m => [E] -> m [TVr]
newVars xs = f xs [] where
    f [] xs = return $ reverse xs
    f (x:xs) ys = do
        s <- newUniq
        f xs (tVr (2*s) x:ys)


tipe t = f t where
    f (TAp t1 t2) = eAp (f t1) (f t2)
    f (TArrow t1 t2) =  EPi (tVr 0 (f t1)) (f t2)
    f (TCon (Tycon n k)) | Just n' <- lookup n primitiveAliases = ELit litCons { litName = n', litType = kind k }
    f (TCon (Tycon n k)) =  ELit litCons { litName = n, litType = kind k }
    f (TVar tv) = EVar (cvar [] tv)
    f (TMetaVar mv) = cmvar mv
    f (TForAll vs (ps :=> t)) = foldr EPi (f t) (map (cvar $ freeVars ps) vs)
    f (TExists xs (_ :=> t)) = let
        xs' = map (kind . tyvarKind) xs
        in ELit litCons { litName = unboxedNameTuple TypeConstructor (length xs' + 1), litArgs = f t:xs', litType = eHash }
    cvar fvs tv@Tyvar { tyvarName = n, tyvarKind = k }
        | tv `elem` fvs = setProperty prop_SCRUTINIZED (tVr (lt n) (kind k))
        | otherwise = tVr (lt n) (kind k)
    cmvar MetaVar { metaKind = k } = tAbsurd (kind k)
    lt n | nameType n == TypeVal = toId n  -- verifies namespace

kind (KBase KUTuple) = eHash
kind (KBase KHash) = eHash
kind (KBase Star) = eStar
kind (KBase KQuest) = eStar      -- XXX why do these still exist?
kind (KBase KQuestQuest) = eStar
kind (Kfun k1 k2) = EPi (tVr 0 (kind k1)) (kind k2)
kind (KVar _) = error "Kind variable still existing."


simplifyDecl (HsPatBind sl (HsPVar n)  rhs wh) = HsFunBind [HsMatch sl n [] rhs wh]
simplifyDecl x = x

simplifyHsPat (HsPInfixApp p1 n p2) = HsPApp n [simplifyHsPat p1, simplifyHsPat p2]
simplifyHsPat (HsPParen p) = simplifyHsPat p
simplifyHsPat (HsPTuple ps) = HsPApp (toTuple (length ps)) (map simplifyHsPat ps)
simplifyHsPat (HsPUnboxedTuple ps) = HsPApp (nameName $ unboxedNameTuple DataConstructor (length ps)) (map simplifyHsPat ps)
simplifyHsPat (HsPNeg p)
    | HsPLit (HsInt i) <- p' = HsPLit $ HsInt (negate i)
    | HsPLit (HsFrac i) <- p' = HsPLit $ HsFrac (negate i)
    | otherwise = HsPNeg p'
    where p' = (simplifyHsPat p)
simplifyHsPat (HsPLit (HsString s)) = simplifyHsPat (HsPList (map f s)) where
    f c = HsPLit (HsChar c)
simplifyHsPat (HsPAsPat n p) = HsPAsPat n (simplifyHsPat p)
simplifyHsPat (HsPTypeSig _ p _) = simplifyHsPat p
simplifyHsPat (HsPList ps) = pl ps where
    pl [] = HsPApp (nameName $ dc_EmptyList) []
    pl (p:xs) = HsPApp (nameName $ dc_Cons) [simplifyHsPat p, pl xs]
simplifyHsPat (HsPApp n xs) = HsPApp n (map simplifyHsPat xs)
simplifyHsPat (HsPIrrPat p) = simplifyHsPat p -- TODO irrefutable patterns!
simplifyHsPat p@HsPVar {} = p
simplifyHsPat p@HsPLit {} = p
simplifyHsPat p = error $ "simplifyHsPat: " ++ show p


fromTyvar (Tyvar _ n k) = tVr (toId n) (kind k)

fromSigma (TForAll vs (_ :=> t)) = (map fromTyvar vs, tipe t)
fromSigma t = ([], tipe t)

convertValue n = do
    assumps <- asks ceAssumps
    dataTable <- asks ceDataTable
    t <- Map.lookup n assumps
    let ty = removeNewtypes dataTable (tipe t)
    cc <- asks ceCoerce
    lm <- case Map.lookup n cc of
        Nothing -> do
            let (vs,_) = fromSigma t
            return (flip (foldr eLam) vs)
        Just CTId -> do return id
        Just (CTAbs ts) -> do return $ \e -> foldr eLam e (map fromTyvar ts)
    return (tVr (toId n) ty,ty,lm)





--convertType t = do
--    dataTable <- asks ceDataTable
--    return $ removeNewtypes dataTable (tipe t)


matchesConv ms = map v ms where
    v (HsMatch _ _ ps rhs wh) = (map simplifyHsPat ps,rhs,wh)

altConv as = map v as where
    v (HsAlt _ p rhs wh) = ([simplifyHsPat p],rhs,wh)

argTypes e = span ((== eBox) . getType) (map tvrType xs) where
    (_,xs) = fromPi e
argTypes' :: E -> ([E],E)
argTypes' e = let (x,y) = fromPi e in (map tvrType y,x)


getMainFunction :: Monad m => DataTable -> Name -> (Map.Map Name (TVr,E)) -> m (Name,TVr,E)
getMainFunction dataTable name ds = do
  mt <- case Map.lookup name ds of
    Just x -> return x
    Nothing -> fail $ "Could not find main function: " ++ show name
  let funcs = runIdentity $ fmapM (\n -> return . fst $ runEither (show n) $ Map.lookup n ds) sFuncNames
  nameToEntryPoint dataTable (fst mt) (toName Name.Val "theMain") Nothing funcs

nameToEntryPoint :: Monad m => DataTable -> TVr -> Name -> Maybe FfiExport -> FuncNames TVr -> m (Name,TVr,E)
nameToEntryPoint dataTable main cname ffi ds = ans where
    ans = do
        let runMain      = func_runMain ds
            runExpr      = func_runExpr ds
            runNoWrapper = func_runNoWrapper ds
            runRaw       = func_runRaw ds
        let e = case ioLike (getType maine) of
                Just x | not (fopts FO.Wrapper) -> EAp (EAp (EVar runNoWrapper) x) maine
                Just x ->  EAp (EAp (EVar runMain)  x ) maine
                Nothing | fopts FO.Raw -> EAp (EAp (EVar runRaw) ty) maine
                Nothing ->  EAp (EAp (EVar runExpr) ty) maine
            ne = ELam worldVar (EAp e (EVar worldVar))
            worldVar = tvr { tvrIdent = 2, tvrType = tWorld__ }
            theMainTvr =  tVr (toId cname) (infertype dataTable ne)
            tvm@(TVr { tvrType =  ty}) =  main
            maine = foldl EAp (EVar tvm) [ tAbsurd k |  TVr { tvrType = k } <- xs, sortStarLike k ]
            (_,xs) = fromPi ty
        return (cname, tvrInfo_u (case ffi of Just ffi -> Info.insert ffi; Nothing -> id) $ setProperty prop_EXPORTED theMainTvr,ne)
    ioLike ty = case ty of
        ELit LitCons { litName = n, litArgs = [x] } | n ==  tc_IO -> Just x
--        (EPi ioc (EPi tvr (ELit LitCons { litName = n, litArgs = [x] }))) | n == tc_IOResult -> Just x
        _ -> Nothing

createInstanceRules :: Monad m => DataTable -> ClassHierarchy -> (Map.Map Name (TVr,E)) -> m Rules
createInstanceRules dataTable classHierarchy funcs = return $ fromRules ans where
    ans = concatMap cClass (classRecords classHierarchy)
    cClass classRecord =  concat [ method classRecord n | (n,TForAll _ (_ :=> t)) <- classAssumps classRecord ]
    method classRecord methodName | isJust _methodName = as where
        methodVar = tVr (toId methodName) ty
        _methodName@(~(Just (TVr {tvrType = ty},_))) = findName methodName
        defaultName =  (defaultInstanceName methodName)
        valToPat' (ELit LitCons { litAliasFor = af,  litName = x, litArgs = ts, litType = t }) = ELit $ litCons { litAliasFor = af, litName = x, litArgs = [ EVar (tVr j (getType z)) | z <- ts | j <- [2,4 ..], j `notElem` map tvrIdent args], litType = t }
        valToPat' (EPi (TVr { tvrType =  a}) b)  = ELit $ litCons { litName = tc_Arrow, litArgs = [ EVar (tVr j (getType z)) | z <- [a,b] | j <- [2,4 ..], j `notElem` map tvrIdent args], litType = eStar }
        valToPat' x = error $ "FromHs.valToPat': " ++ show x
        as = [ rule  t | Inst { instHead = _ :=> IsIn _ t }  <- snub (classInsts classRecord) ]
        (_ft,_:args') = fromPi ty
        (args,_rargs) = span (sortStarLike . getType)  args'
        rule t = emptyRule { ruleHead = methodVar, ruleArgs = tpat:map EVar args, ruleBinds = [ t | ~(EVar t) <- vs] ++ args, ruleBody = removeNewtypes dataTable body, ruleUniq = (Module (show name),0), ruleName = toAtom $ "Rule.{" ++ show name ++ "}"}  where
            tpat = valToPat' (removeNewtypes dataTable $ tipe t)
            name = (instanceName methodName (getTypeCons t))
            vp@(ELit LitCons { litArgs =  vs }) = tpat
            body = case findName name of
                Just (n,_) -> foldl EAp (EVar n) (vs ++ map EVar args)
                Nothing -> case findName defaultName of
                    Just (deftvr,_) | null vs -> foldl EAp (EAp (EVar deftvr) vp) (map EVar args)
                    Just (deftvr,_) -> eLet tv vp $ foldl EAp (EAp (EVar deftvr) (EVar tv)) (map EVar args) where
                        tv = tvr { tvrIdent = head [ n | n <- [2,4..], n `notElem` freeVars vp], tvrType = getType vp }
                    Nothing -> foldl EAp (EError ( show methodName ++ ": undefined at type " ++  PPrint.render (pprint t)) (eAp ty (valToPat' (tipe t)))) (map EVar args)
    method _ _ = []
    findName name = case Map.lookup name funcs of
        Nothing -> fail $ "Cannot find: " ++ show name
        Just n -> return n

getTypeCons (TCon (Tycon n _)) = n
getTypeCons (TAp a _) = getTypeCons a
getTypeCons (TArrow {}) = tc_Arrow
getTypeCons x = error $ "getTypeCons: " ++ show x



unbox :: DataTable -> E -> Int -> (TVr -> E) -> E
unbox dataTable e vn wtd = eCase e [Alt (litCons { litName = cna, litArgs = [tvra], litType = te }) (wtd tvra)] Unknown where
    te = getType e
    tvra = tVr vn sta
    Just (cna,sta,ta) = lookupCType' dataTable te

createFunc :: UniqueProducer m => DataTable -> [E] -> ([(TVr,String)] -> (E -> E,E)) -> m E
createFunc dataTable es ee = do
    xs <- flip mapM es $ \te -> do
        res@(_,sta,rt) <- lookupCType' dataTable te
        [n,n'] <- newVars [te,sta]
        return (n,(n',rt),res)
    let tvrs' = [ n' | (_,n',_) <- xs ]
        tvrs = [ t | (t,_,_) <- xs]
        (me,innerE) = ee tvrs'
        eee = me $ foldr esr innerE xs
        esr (tvr,(tvr',_),(cn,_,_)) e = eCase (EVar tvr) [Alt (litCons { litName = cn, litArgs = [tvr'], litType = te }) e] Unknown  where
            te = getType $ EVar tvr
    return $ foldr ELam eee tvrs

instance GenName String where
   genNames i = map (('x':) . show) [i..]

convertRules :: TiData -> ClassHierarchy -> Map.Map Name Type -> DataTable -> [HsDecl] -> IO [(String,[TVr],E,E)]
convertRules tiData classHierarchy assumps dataTable hsDecls = concatMapM g hsDecls where
    g (HsPragmaRules rs) = mapM f rs
    g _ = return []
    f pr = do
        let ce = convertE tiData classHierarchy assumps dataTable (hsRuleSrcLoc pr)
        e1 <- ce (hsRuleLeftExpr pr)
        e2 <- ce (hsRuleRightExpr pr)
        (ts,cs) <- runNameMT $ do
            ts <- flip mapM (filter (sortStarLike . getType) $ freeVars e1) $ \tvr -> do
                --return (tvrIdent tvr,tvr)
                nn <- newNameFrom (map (:'\'':[]) ['a' ..])
                return (tvrIdent tvr,tvr { tvrIdent = toId (toName TypeVal nn) })
            cs <- flip mapM [toTVr assumps (toName Val v) | (v,_) <- hsRuleFreeVars pr ] $ \tvr -> do
                let ur = show $ unRename $ nameName (toUnqualified $ runIdentity $ fromId (tvrIdent tvr))
                nn <- newNameFrom (ur:map (\v -> ur ++ show v) [1 ::Int ..])
                return (tvrIdent tvr,tvr { tvrIdent = toId (toName Val nn) })
            return (ts,cs)
        let smt = substMap $ fromList [ (x,EVar y)| (x,y) <- ts ]
            sma = substMap $ fromList [ (x,EVar y)| (x,y) <- cs' ]
            cs' =  [ (x,(tvrType_u smt y))| (x,y) <- cs ]
            e2' = deNewtype dataTable $ smt $ sma e2
        e2 <- atomizeAp False dataTable Stats.theStats mainModule e2'
        return (hsRuleString pr,( snds (cs' ++ ts) ),eval $ smt $ sma e1,e2)

convertE :: Monad m => TiData -> ClassHierarchy -> Map.Map Name Type -> DataTable -> SrcLoc -> HsExp -> m E
convertE tiData classHierarchy assumps dataTable srcLoc exp = do
    [(_,_,e)] <- convertDecls tiData classHierarchy assumps dataTable [HsPatBind srcLoc (HsPVar sillyName') (HsUnGuardedRhs exp) []]
    return e

sillyName' = nameName v_silly

data CeEnv = CeEnv {
    ceAssumps :: Map.Map Name Type,
    ceCoerce :: Map.Map Name CoerceTerm,
    ceDataTable :: DataTable
    }

newtype Ce t a = Ce (RWST CeEnv () Int t a)
    deriving(Monad,Functor,MonadTrans,MonadIO,MonadReader CeEnv,MonadState Int)

instance Monad m => UniqueProducer (Ce m) where
    newUniq = do
        i <- get
        put $! (i + 1)
        return i

instance Monad m => DataTableMonad (Ce m) where
    getDataTable = asks ceDataTable

applyCoersion :: Monad m => CoerceTerm -> E -> Ce m E
applyCoersion CTId e = return e
applyCoersion ct e = etaReduce `liftM` f ct e where
    f CTId e = return e
    f (CTAp ts) e = return $ foldl eAp e (map tipe ts)
    f (CTAbs ts) e = return $ foldr eLam e (map fromTyvar ts)
    f (CTCompose ct1 ct2) e = f ct1 =<< (f ct2 e)
    f (CTFun CTId) e = return e
    f (CTFun ct) e = do
        let EPi TVr { tvrType = ty } _ = getType e
        [y] <- newVars [ty]
        fgy <- f ct (EAp e (EVar y))
        return (eLam y fgy)

-- | return primitive instances associated with class given as argument
primitiveInstances :: Name -> [(Name,TVr,E)]
primitiveInstances name = [(n,setProperties [prop_INSTANCE,prop_INLINE] $ tVr (toId n) (getType v),v) | (cn,n,v) <- constantMethods, cn == name]

convertDecls :: Monad m => TiData -> ClassHierarchy -> Map.Map Name Type -> DataTable -> [HsDecl] -> m [(Name,TVr,E)]
convertDecls tiData classHierarchy assumps dataTable hsDecls = liftM fst $ evalRWST ans ceEnv 2 where
    ceEnv = CeEnv {
        ceCoerce = tiCoerce tiData,
        ceAssumps = assumps,
        ceDataTable = dataTable
        }
    Ce ans = do
        nds <- mapM cDecl hsDecls
        return (map anninst $ concat nds)
    doNegate e = eAp (eAp (func_negate funcs) (getType e)) e
    Identity funcs = fmapM (return . EVar . toTVr assumps) sFuncNames
    anninst (a,b,c)
        | "Instance@" `isPrefixOf` show a = (a,setProperty prop_INSTANCE b, deNewtype dataTable c)
        | otherwise = (a,b, deNewtype dataTable c)
    cDecl :: Monad m => HsDecl -> Ce m [(Name,TVr,E)]
    cDecl (HsForeignDecl _ (FfiSpec (Import cn req) _ Primitive) n _) = do
        let name      = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        let (ts,rt)   = argTypes' ty
            prim      = APrim (PrimPrim cn) req
        es <- newVars [ t |  t <- ts, not (sortStarLike t) ]
        let result    = foldr ($) (processPrimPrim dataTable $ EPrim prim (map EVar es) rt) (map ELam es)
        return [(name,var,lamt result)]
    cDecl (HsForeignDecl _ (FfiSpec (ImportAddr rcn req) _ _) n _) = do
        let name       = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        let (ts,rt)    = argTypes' ty
        (cn,st,ct) <- lookupCType' dataTable rt
        [uvar] <- newVars [st]
        let expr x     = return [(name,var,lamt x)]
            prim       = APrim (AddrOf rcn) req
        expr $ eStrictLet uvar (EPrim prim [] st) (ELit (litCons { litName = cn, litArgs = [EVar uvar], litType = rt }))
    cDecl (HsForeignDecl _ (FfiSpec (Import rcn req) _ CCall) n _) = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        let (ts,rt) = argTypes' ty
            (isIO,rt') = case  rt of
                ELit (LitCons { litName = c, litArgs = [x] }) | c == tc_IO -> (True,x)
                _ -> (False,rt)
        es <- newVars [ t |  t <- ts, not (sortStarLike t) ]
        (_,pt) <- lookupCType dataTable rt'
        [tvrWorld, tvrWorld2] <- newVars [tWorld__,tWorld__]
        let cFun = createFunc dataTable (map tvrType es)
            prim io rs rtt = EPrim (APrim (Func io rcn (snds rs) rtt) req)
        result <- case (isIO,pt) of
            (True,"void") -> cFun $ \rs -> (,) (ELam tvrWorld) $
                        eStrictLet tvrWorld2 (prim True rs "void" (EVar tvrWorld:[EVar t | (t,_) <- rs ]) tWorld__) (eJustIO (EVar tvrWorld2) vUnit)
            (False,"void") -> fail "pure foreign function must return a valid value"
            _ -> do
                (cn,rtt',rtt) <- lookupCType' dataTable rt'
                [rtVar,rtVar'] <- newVars [rt',rtt']
                let rttIO = ltTuple [tWorld__, rt']
                    rttIO' = ltTuple' [tWorld__, rtt']
                case isIO of
                    False -> cFun $ \rs -> (,) id $ eStrictLet rtVar' (prim False rs rtt [ EVar t | (t,_) <- rs ] rtt') (ELit $ litCons { litName = cn, litArgs = [EVar rtVar'], litType = rt' })
                    True -> cFun $ \rs -> (,) (ELam tvrWorld) $
                                eCaseTup' (prim True rs rtt (EVar tvrWorld:[EVar t | (t,_) <- rs ]) rttIO')  [tvrWorld2,rtVar'] (eLet rtVar (ELit $ litCons { litName = cn, litArgs = [EVar rtVar'], litType = rt' }) (eJustIO (EVar tvrWorld2) (EVar rtVar)))
        return [(name,var,lamt result)]

    cDecl x@HsForeignDecl {} = fail ("Unsupported foreign declaration: "++ show x)
    cDecl (HsForeignExport _ ffi@(FfiExport ecn _ CCall) n _) = do
        return . (:[]) =<< nameToEntryPoint dataTable (tv n) (toName Name.FfiExportName ecn) (Just ffi) =<< fmapM (return . toTVr assumps) sFuncNames
    cDecl x@HsForeignExport {} = fail ("Unsupported foreign export: "++ show x)

    cDecl (HsPatBind sl p (HsUnGuardedRhs exp) []) | (HsPVar n) <- simplifyHsPat p, n == sillyName' = do
        e <- cExpr exp
        return [(v_silly,tvr,e)]

    cDecl (HsPatBind sl p rhs wh) | (HsPVar n) <- simplifyHsPat p = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        rhs <- cRhs sl rhs
        lv <- hsLetE wh rhs
        return [(name,var,lamt lv)]
    cDecl (HsFunBind [(HsMatch sl n ps rhs wh)]) | ps' <- map simplifyHsPat ps, all isHsPVar ps' = do
        let name = toName Name.Val n
        (var,ty,lamt) <- convertValue name
        rhs <- cRhs sl rhs
        lv <- hsLetE wh rhs
        return [(name,var,lamt $ lp  ps' lv)]
    cDecl (HsFunBind ms@((HsMatch sl n ps _ _):_)) = do
        let name = toName Name.Val n
        (var,t,lamt) <- convertValue name
        let (targs,eargs) = argTypes t
            numberPatterns = length ps
        bs' <- newVars (take numberPatterns eargs)
        let bs  = map EVar bs'
            rt = discardArgs (length targs + numberPatterns) t
            z e = foldr eLam e bs'
        ms <- cMatchs bs (matchesConv ms) (ump sl rt)
        return [(name,var,lamt $ z ms )]
    cDecl HsNewTypeDecl {  hsDeclName = dname, hsDeclArgs = dargs, hsDeclCon = dcon, hsDeclDerives = derives } = return $ makeDerives dname dargs [dcon] (map (toName ClassName) derives)
    cDecl HsDataDecl {  hsDeclName = dname, hsDeclArgs = dargs, hsDeclCons = dcons, hsDeclDerives = derives } = return $ makeDerives dname dargs dcons (map (toName ClassName) derives)
    cDecl cd@(HsClassDecl {}) = cClassDecl cd
    cDecl _ = return []
    makeDerives dname dargs dcons derives  = concatMap f derives where
        f n | n == class_Bounded, all (null . hsConDeclArgs) dcons  = []
        f _ = []
    cExpr :: Monad m => HsExp -> Ce m E
    cExpr (HsAsPat n' (HsCon n)) = return $ constructionExpression dataTable (toName DataConstructor n) rt where
        t' = getAssump n'
        (_,rt) = argTypes' (tipe t')
    cExpr (HsLit (HsString s)) = return $ E.Values.toE s
    cExpr (HsAsPat n' (HsLit (HsInt i))) = ans where
        t' = getAssump n'
        ty = tipe t'
        ans = case lookupCType' dataTable ty of
            Just (cn,st,_it) -> return $ ELit (litCons { litName = cn, litArgs = [ELit (LitInt (fromIntegral i) st)], litType = ty })
            Nothing -> return $ intConvert' funcs ty i
            --Just (cn,st,it) ->
    --cExpr (HsLit (HsInt i)) = return $ intConvert i
    cExpr (HsLit (HsChar ch)) = return $ toE ch
    cExpr (HsLit (HsFrac i))  = return $ toE i
    cExpr (HsLambda sl ps e) | all isHsPVar ps' = do
        e <- cExpr e
        return $ lp ps' e
      where ps' = map simplifyHsPat ps
    cExpr (HsInfixApp e1 v e2) = do
        v <- cExpr v
        e1 <- cExpr e1
        e2 <- cExpr e2
        return $ eAp (eAp v e1) e2
    cExpr (HsLeftSection op e) = liftM2 eAp (cExpr op) (cExpr e)
    cExpr (HsApp (HsRightSection e op) e') = do
        op <- cExpr op
        e' <- cExpr e'
        e <- cExpr e
        return $ eAp (eAp op e') e
    cExpr (HsRightSection e op) = do
        cop <- cExpr op
        ce <- cExpr e
        let (_,TVr { tvrType = ty}:_) = fromPi (getType cop)
        [var] <- newVars [ty]
        return $ eLam var (eAp (eAp cop (EVar var)) ce)
    cExpr (HsApp e1 e2) = liftM2 eAp (cExpr e1) (cExpr e2)
    cExpr (HsParen e) = cExpr e
    cExpr (HsExpTypeSig _ e _) = cExpr e
    cExpr (HsNegApp e) = liftM doNegate (cExpr e)
    cExpr (HsLet dl e) = hsLet dl e
    cExpr (HsIf e a b) = join $ liftM3 createIf (cExpr e) (cExpr a) (cExpr b)
    cExpr (HsCase _ []) = error "empty case"
    cExpr (HsAsPat n HsError { hsExpString = msg }) = do
        let ty = cType n
        return $ EError msg ty
    cExpr (HsAsPat n hs@(HsCase e alts)) = do
        let ty = cType n
        scrut <- cExpr e
        cMatchs [scrut] (altConv alts) (EError ("No Match in Case expression at " ++ show (srcLoc hs))  ty)
    cExpr (HsTuple es) = liftM eTuple (mapM cExpr es)
    cExpr (HsUnboxedTuple es) = liftM eTuple' (mapM cExpr es)
    cExpr (HsAsPat n (HsList xs)) = do
        let cl (x:xs) = liftM2 eCons (cExpr x) (cl xs)
            cl [] = return $ eNil (cType n)
        cl xs
    cExpr (HsVar n) = do
        return (EVar (tv n))
    cExpr (HsAsPat n' e) = do
        e <- cExpr e
        cc <- asks ceCoerce
        case Map.lookup (toName Val n') cc of
            Nothing -> return e
            Just c -> applyCoersion c e
    cExpr e = fail ("Cannot convert: " ++ show e)
    hsLetE [] e = return  e
    hsLetE dl e = do
        nds <- mconcatMapM cDecl dl
        return $ eLetRec [ (b,c) | (_,b,c) <- nds] e
    hsLet dl e = do
        e <- cExpr e
        hsLetE dl e

    cMatchs :: Monad m => [E] -> [([HsPat],HsRhs,[HsDecl])] -> E -> Ce m E
    cMatchs bs ms els = do
        pg <- processGuards ms
        convertMatches funcs tv cType bs pg els

    cGuard (HsUnGuardedRhs e) = liftM const $ cExpr e
    cGuard (HsGuardedRhss (HsGuardedRhs _ g e:gs)) = do
        g <- cExpr g
        e <- cExpr e
        fg <- cGuard (HsGuardedRhss gs)
        [nv] <- newVars [Unknown]
        return (\els -> createIfv nv g e (fg els))
    cGuard (HsGuardedRhss []) = return id

    getAssump n  = case Map.lookup (toName Name.Val n) assumps of
        Just z -> z
        Nothing -> error $ "Lookup failed: " ++ (show n)
    tv n = tvr { tvrType = removeNewtypes dataTable (tvrType tvr) } where
        tvr = toTVr assumps (toName Name.Val n)
    lp  [] e = e
    lp  (HsPVar n:ps) e = eLam (tv n) $ lp  ps e
    lp  p e  =  error $ "unsupported pattern:" <+> tshow p  <+> tshow e
    cRhs sl (HsUnGuardedRhs e) = cExpr e
    cRhs sl (HsGuardedRhss []) = error "HsGuardedRhss: empty"
    cRhs sl (HsGuardedRhss gs@(HsGuardedRhs _ _ e:_)) = f gs where
        f (HsGuardedRhs _ g e:gs) = join $ liftM3 createIf (cExpr g) (cExpr e) (f gs)
        f [] = do
            e <- cExpr e
            return $ ump sl $ getType e
    processGuards xs = flip mapM xs $ \ (ps,e,wh) -> do
        cg <- cGuard e
        nds <- mconcatMapM cDecl wh
        let elet = eLetRec [ (b,c) | (_,b,c) <- nds]
        return (map simplifyHsPat ps,elet . cg )
    cType (n::HsName) = fst $ convertVal assumps (toName Name.Val n)

    cClassDecl (HsClassDecl _ (HsQualType _ (HsTyApp (HsTyCon name) _)) decls) = do
        let ds = map simplifyDecl decls
            cr = findClassRecord classHierarchy className
            className = (toName ClassName name)
            cClass classRecord =  [ f n (toId n) (removeNewtypes dataTable $ tipe t) | (n,t) <- classAssumps classRecord ] where
                f n i t = (n,setProperties [prop_METHOD,prop_PLACEHOLDER] $ tVr i t, foldr ELam (EPrim (primPrim ("Placeholder: " ++ show n)) [] ft) args)  where
                    (ft',as) = fromPi t
                    (args,rargs) = span (sortStarLike . getType) as
                    ft = foldr EPi ft' rargs
        return (cClass cr ++ primitiveInstances className)
    cClassDecl _ = error "cClassDecl"

toTVr assumps n = tVr (toId n) (typeOfName n) where
    typeOfName n = fst $ convertVal assumps n
convertVal assumps n = (foldr ePi t vs, flip (foldr eLam) vs) where
    (vs,t) = case Map.lookup n assumps of
        Just z -> fromSigma  z
        Nothing -> error $ "convertVal.Lookup failed: " ++ (show n)


integer_cutoff = 500000000

intConvert i | abs i > integer_cutoff  =  ELit (litCons { litName = dc_Integer, litArgs = [ELit $ LitInt (fromInteger i) (rawType "intmax_t")], litType = tInteger })
intConvert i =  ELit (litCons { litName = dc_Int, litArgs = [ELit $ LitInt (fromInteger i) (rawType "int")], litType = tInt })

intConvert' funcs typ i = EAp (EAp fun typ) (ELit (litCons { litName = con, litArgs = [ELit $ LitInt (fromInteger i) (rawType rawtyp)], litType = ltype }))  where
    (con,ltype,fun,rawtyp) = case abs i > integer_cutoff of
        True -> (dc_Integer,tInteger,f_fromInteger,"intmax_t")
        False -> (dc_Int,tInt,f_fromInt,"int")
    f_fromInt = func_fromInt funcs
    f_fromInteger = func_fromInteger funcs

litconvert (HsChar i) t | t == tChar =  LitInt (fromIntegral $ ord i) tCharzh
litconvert e t = error $ "litconvert: shouldn't happen: " ++ show (e,t)


fromHsPLitInt (HsPLit l@(HsInt _)) = return l
fromHsPLitInt (HsPLit l@(HsFrac _)) = return l
fromHsPLitInt x = fail $ "fromHsPLitInt: " ++ show x

convertMatches funcs tv cType bs ms err = match bs ms err where
    doNegate e = eAp (eAp (func_negate funcs) (getType e)) e
    fromInt = func_fromInt funcs
    fromInteger = func_fromInteger funcs
    fromRational = func_fromRational funcs
    match :: Monad m => [E] -> [([HsPat],E->E)] -> E -> Ce m E
    match  [] ps err = f ps where
        f (([],e):ps) = do
            r <- f ps
            return (e r)
        f [] = return err
        f _ = error "FromHs.convertMatches.match"
    match _ [] err = return err
    match (b:bs) ps err = f patternGroups err where
        isJoinPoint (EAp (EVar x) _) | getProperty prop_JOINPOINT x = True
        isJoinPoint _ = False
        f  [] err = return err
        f (ps:pss) err = do
            err' <- f pss err
            if isEVar err' || isEError err' || isJoinPoint err' then
               g ps err'
               else do
                [ev] <- newVars [EPi tvr { tvrType = unboxedTyUnit } $ getType err']
                let ev' = setProperties [prop_ONESHOT, prop_JOINPOINT] ev
                nm <- g ps (EAp (EVar ev') unboxedUnit)
                return $ eLetRec [(ev',ELam (setProperty prop_ONESHOT tvr { tvrType = unboxedTyUnit }) err')] nm
        g ps err
            | all (not . isStrictPat) patternHeads = match bs [(ps',eLetRec (toBinding p) . e)  | (p:ps',e) <- ps] err
            | any (isHsPAsPat || isHsPNeg || isHsPIrrPat) patternHeads = g (map (procAs b) ps) err
            | Just () <- mapM_ fromHsPLitInt patternHeads = do
                let tb = getType b
                [bv] <- newVars [tb]
                let gps = [ (p,[ (ps,e) |  (_:ps,e) <- xs ]) | (p,xs) <- sortGroupUnderF ((\ (x:_) -> x) . fst) ps]
                    eq = EAp (func_equals funcs) tb
                    f els (HsPLit (HsInt i),ps) = do
                        --let ip = (EAp (EAp fromInt tb) (ELit (LitInt (fromIntegral i) tInt)))
                        let ip | abs i > integer_cutoff  = (EAp (EAp fromInteger tb) (intConvert i))
                               | otherwise =  (EAp (EAp fromInt tb) (intConvert i))
                        m <- match bs ps err
                        createIf (EAp (EAp eq (EVar bv)) ip) m els
                    f els (HsPLit (HsFrac i),ps) = do
                        --let ip = (EAp (EAp fromInt tb) (ELit (LitInt (fromIntegral i) tInt)))
                        let ip = (EAp (EAp fromRational tb) (toE i))
                        m <- match bs ps err
                        createIf (EAp (EAp eq (EVar bv)) ip) m els
                e <- foldlM f err gps
                return $ eLetRec [(bv,b)] e
            | all isHsPLit patternHeads = do
                let gps = [ (p,[ (ps,e) |  (_:ps,e) <- xs ]) | (p,xs) <- sortGroupUnderF ((\ (x:_) -> x) . fst) ps]
                    f (HsPLit l,ps) = do
                        m <- match bs ps err
                        return (Alt  (litconvert l (getType b)) m)
                as@(_:_) <- mapM f gps
                [TVr { tvrIdent = vr }] <- newVars [Unknown]
                dataTable <- asks ceDataTable
                return $ unbox dataTable b vr $ \tvr -> eCase (EVar tvr) as err
                --return $ eCase b as err
            | all isHsPApp patternHeads = do
                dataTable <- getDataTable
                let gps =  sortGroupUnderF (hsPatName . (\ (x:_) -> x) . fst) ps
                    (Just patCons) = getConstructor (toName DataConstructor $ fst $ head gps) dataTable
                    f (name,ps) = do
                        let spats = hsPatPats $ (\ (x:_) -> x) $ fst ((\ (x:_) -> x) ps)
                            nargs = length spats
                        vs <- newVars (slotTypes dataTable (toName DataConstructor name) (getType b))
                        vs' <- newVars (map (const Unknown) vs)

                        ps' <- mapM pp ps
                        m <- match (map EVar vs ++ bs) ps' err
                        return $ deconstructionExpression dataTable (toName DataConstructor name) (getType b) vs vs' m
                        --return (Alt (LitCons (toName DataConstructor name) vs (getType b))  m)
                    --pp :: Monad m =>  ([HsPat], E->E) -> m ([HsPat], E->E)
                    pp (HsPApp n ps:rps,e)  = do
                        return $ (ps ++ rps , e)
                as@(_:_) <- mapM f gps
                case conVirtual patCons of
                    Nothing -> return $ eCase b as err
                    Just sibs -> do
                        let (Just Constructor { conChildren = Just [vCons] }) = getConstructor (conInhabits patCons) dataTable
                        [z] <- newVars [tIntzh]
                        let err' = if length sibs <= length as then Unknown else err
                        return $ eCase b [Alt litCons { litName = vCons, litArgs = [z], litType = getType b } (eCase (EVar z) as err')] Unknown
            | otherwise = error $ "Heterogenious list: " ++ show patternHeads
            where
            patternHeads = map ((\ (x:_) -> x) . fst) ps
        patternGroups = groupUnder (isStrictPat . (\ (x:_) -> x) . fst) ps
        procAs b (HsPNeg p:ps, ef) =  (p:ps,ef)  -- TODO, negative patterns
        procAs b (HsPAsPat n p:ps, ef) =  (p:ps,eLetRec [((tv n),b)] . ef)
        procAs b (HsPIrrPat p:ps, ef) =  (p:ps, ef) -- TODO, irrefutable patterns
        procAs _ x = x
        toBinding (HsPVar v) = [(tv v,b)]
        toBinding (HsPNeg (HsPVar v)) = [(tv v,doNegate b)]
        toBinding (HsPIrrPat p) = toBinding p
        toBinding (HsPAsPat n p) = (tv n,b):toBinding p
        toBinding p = error $ "toBinding: " ++ show p



isStrictPat HsPVar {} = False
isStrictPat (HsPNeg p) = isStrictPat p
isStrictPat (HsPAsPat _ p) = isStrictPat p
isStrictPat (HsPIrrPat p) = isStrictPat p  -- TODO irrefutable patterns
isStrictPat _ = True



specializeE :: Monad m
    => E   -- ^ the general type
    -> E   -- ^ the specific type
    -> m [E]  -- ^ what to apply the general type to to get the specific one
specializeE gt st = do
    let f zs x | Just mm <- match (const Nothing) zs x st = mapM (g mm) (reverse zs) where
            g mm tvr = case lookup tvr mm of
                Just x -> return x
                Nothing -> fail $ "specializeE: variable not bound: " ++ pprint (((gt,st),(mm,tvr)),(zs,x))
        f zs (EPi vbind exp) = f (vbind:zs) exp
        f _ _ = fail "specializeE: attempt to specialize types that do not unify"
    f [] gt



makeSpec :: Monad m => (TVr,E) -> T.Rule -> m ((TVr,E),Rule)
makeSpec (t,e) T.RuleSpec { T.ruleType = rt, T.ruleUniq = (Module m,ui), T.ruleSuper = ss } = do
    let nt = tipe rt
    as <- specializeE (getType t) nt
    let ntvr = tvr { tvrIdent = toId newName, tvrType = nt, tvrInfo = setProperties (prop_SPECIALIZATION:sspec) mempty }
        Just nn = fromId (tvrIdent t)
        (ntype,Just m,q) = nameParts nn
        newName = toName ntype (Just $ "Spec@." ++ m ++ "." ++ show ui,'f':m ++ "." ++ q)
        sspec = if ss then [prop_SUPERSPECIALIZE] else []
        ar = makeRule ("Specialize.{" ++ show newName) (Module m,ui) [] t as (EVar ntvr)
    return ((ntvr,foldl EAp e as),ar)


deNewtype :: DataTable -> E -> E
deNewtype dataTable e = removeNewtypes dataTable (f e) where
    f ECase { eCaseScrutinee = e, eCaseAlts =  ((Alt (LitCons { litName = n, litArgs = [v], litType = t }) z):_) } | alias == ErasedAlias = f (eLet v e z) where
        Identity Constructor { conAlias = alias } = getConstructor n dataTable
    f ECase { eCaseScrutinee = e, eCaseAlts =  ((Alt (LitCons { litName = n, litArgs = [v], litType = t }) z):_) } | alias == RecursiveAlias = f $ eLet v (prim_unsafeCoerce e (getType v)) z where
        Identity Constructor { conAlias = alias } = getConstructor n dataTable
    f e = runIdentity $ emapE (return . f) e



