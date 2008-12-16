
module C.FromGrin2(compileGrin) where

import Control.Monad.Identity
import Control.Monad.RWS
import System.IO.Unsafe
import Data.List
import Data.Char
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen(indent,(<$$>))
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.ANSI.Leijen as P

import Data.DeriveTH
import Data.Derive.All

import C.Arch
import C.FFI
import C.Generate
import C.Prims
import Cmm.Number
import Doc.DocLike
import Doc.PPrint
import Grin.Grin
import Grin.HashConst
import Grin.Noodle
import Grin.Show()
import Grin.Val
import Options
import PackedString
import RawFiles
import StringTable.Atom
import Support.CanType
import Support.FreeVars
import Util.Gen
import Util.UniqueMonad
import qualified Cmm.Op as Op
import qualified FlagOpts as FO

---------------
-- C Monad
---------------

data Todo = TodoReturn | TodoExp [Expression] | TodoDecl Name Type | TodoNothing


data Written = Written {
    wRequires :: Requires,
    wStructures :: Map.Map Name Structure,
    wTags :: Set.Set Atom,
    wEnums :: Map.Map Name Int,
    wFunctions :: Map.Map Name Function
    }
$(derive makeMonoid ''Written)

data Env = Env {
    rTodo :: Todo,
    rInscope :: Set.Set Name,
    rDeclare :: Bool,
    rEMap :: Map.Map Atom (Name,[Expression]),
    rCPR  :: Set.Set Atom,
    rGrin :: Grin
    }
$(derive makeUpdate ''Env)


newtype C a = C (RWST Env Written HcHash Uniq a)
    deriving(Monad,UniqueProducer,MonadState HcHash,MonadWriter Written,MonadReader Env,Functor)


runC :: Grin -> C a -> (a,HcHash,Written)
runC grin (C m) =  execUniq1 (runRWST m Env { rCPR = cpr, rGrin = grin, rDeclare = False, rTodo = TodoExp [], rEMap = mempty, rInscope = mempty } emptyHcHash) where
    TyEnv tmap = grinTypeEnv grin
    cpr = iw `Set.union` Set.insert cChar (Set.fromList [ a | (a,TyTy { tySlots = [s], tySiblings = Just [a'] }) <- Map.assocs tmap, a == a', isJust (good s) ])
    iw = if fopts FO.FullInt then Set.empty else Set.fromList [cInt, cWord]
    good s = do
        ct <- Op.toCmmTy s
        b <- Op.cmmTyBits ct
        guard $ b <= 30
        Op.HintNone <- Op.cmmTyHint ct
        return True

tellFunctions :: [Function] -> C ()
tellFunctions fs = tell mempty { wFunctions = Map.fromList $ map (\x -> (functionName x,x)) fs }

localTodo :: Todo -> C a -> C a
localTodo todo (C act) = C $ local (\ r -> r { rTodo = todo }) act


--------------
-- entry point
--------------

{-# NOINLINE compileGrin #-}
compileGrin :: Grin -> (String,[String])
compileGrin grin = (hsffi_h ++ lhc_rts_header_h ++ lhc_rts_alloc_c ++ lhc_rts_c ++ lhc_rts2_c ++ generateArchAssertions ++ show ans ++ "\n", snub (reqLibraries req))  where
    ans = vcat $ includes ++ [text "", enum_tag_t, header, cafs,buildConstants grin finalHcHash, body] :: P.Doc
    includes =  map include (snub $ reqIncludes req)
    include fn = text "#include <" <> text fn <> text ">"
    (header,body) = generateC (Map.elems fm) (Map.elems sm)
    ((),finalHcHash,Written { wRequires = req, wFunctions = fm, wEnums = wenum, wStructures = sm, wTags = ts }) = runC grin go
    enum_tag_t | null enums = mempty
               | otherwise  = text "enum {" <$$> indent 4 (P.vcat (punctuate P.comma $ enums)) <$$> text "};"
        where
            f t n = tshow t <> text " = " <> tshow (n :: Int)
            enums =  map (uncurry f) (Map.toList wenum) ++ (zipWith f (Set.toList (Set.map nodeTagName ts)) [0 ..])
    go = do
        funcs <- liftM concat $ flip mapM (grinFuncs grin) $ \(a,l) -> do
                    convertFunc (Map.lookup a (grinEntryPoints grin)) (a,l)
        tellFunctions funcs
        h <- get
        let tset = Set.fromList [ n | (HcNode n (_:_),_) <- hconsts]
            tset' = Set.fromList [ n | (HcNode n [],_) <- hconsts]
            hconsts = Grin.HashConst.toList h
        mapM_ tellAllTags [ v  | (HcNode _ vs,_) <- hconsts, Left v <- vs]
        mapM_ declareStruct  (Set.toList tset)
        mapM_ tellTags (Set.toList $ tset `mappend` tset')
    cafs = text "/* CAFS */" <$$> (vcat $ map ccaf (grinCafs grin))

convertFunc :: Maybe (FfiExport, ([ExtType], ExtType)) -> (Atom,Lam) -> C [Function]
convertFunc ffie (n,as :-> body) = do
        s <- localTodo TodoReturn (convertBody body)
        let bt = getType body
            mmalloc [TyPtr TyNode] = [a_MALLOC]
            mmalloc [TyNode] = [a_MALLOC]
            mmalloc _ = []
            ats = a_STD:mmalloc bt
            fnname = nodeFuncName n

        fr <- convertTypes bt
        as' <- flip mapM (zip [1 :: Int .. ] as) $ \ (ix,(Var v t)) -> do
            t' <- convertType t
            return $ if v == v0 then (name $ 'u':show ix,t') else (varName v,t')

        mstub <- case ffie of
                Nothing -> return []
                Just ~(FfiExport cn Safe CCall, (argTys, retTy)) -> do
                    newVars <- mapM (liftM (name . show) . newVar . basicType) argTys

                    let fnname2 = name cn
                        as2 = zip (newVars) (map basicType argTys)
                        fr2 = basicType retTy

                    return [function fnname2 fr2 as2 [Public]
                                     (creturn $ cast fr2 $ functionCall fnname $
                                      zipWith cast (map snd as')
                                                   (map variable newVars))]

        return (function fnname fr as' ats (profile_function_inc & s) : mstub)


fetchVar :: Var -> Ty -> C Expression
fetchVar (V 0) _ = return $ noAssign (err "fetchVar v0")
fetchVar v@(V n) _ | n < 0 = return $ (variable  $ varName v)
fetchVar v ty = do
    t <- convertType ty
    is <- asks rInscope
    let n = varName v
    dclare <- asks rDeclare
    return $ (if v == v0 then noAssign else id) $ if not dclare then variable n else localVariable t n

fetchVar' :: Var -> Ty -> C (Name,Type)
fetchVar' (V n) _ | n < 0 = error "fetchVar': CAF"
fetchVar' v ty = do
    t <- convertType ty
    return $ (varName v,t)

convertVals :: [Val] -> C Expression
convertVals [] = return emptyExpression
convertVals [x] = convertVal x
convertVals xs = do
    ts <- mapM convertType (map getType xs)
    xs <- mapM convertVal xs
    return (structAnon (zip xs ts))

convertVal :: Val -> C Expression
convertVal v = cvc v where
    cvc v = convertConst v >>= maybe (cv v) return
    cv (Var v ty) = fetchVar v ty
    cv (Const h) = do
        cpr <- asks rCPR
        case h of
            NodeC a ts -> do
                bn <- basicNode a ts
                case bn of
                    Just bn ->  return (cast sptr_t bn)
                    _ -> do
                        (_,i) <- newConst cpr h
                        return $ variable (name $  'c':show i )
            _ -> do
                (_,i) <- newConst cpr h
                return $ variable (name $  'c':show i )
    cv h@(NodeC a ts) | valIsConstant h = do
        cpr <- asks rCPR
        bn <- basicNode a ts
        case bn of
            Just bn -> return bn
            _ -> do
                (_,i) <- newConst cpr h
                return $ f_PROMOTE (variable (name $  'c':show i ))

    cv (ValPrim (APrim p _) [x] (TyPrim opty)) = do
        x' <- convertVal x
        case p of
            Op (Op.UnOp n ta) r -> primUnOp n ta r x'
            Op (Op.ConvOp n ta) r -> return $ castFunc n ta r x'
            x -> return $ err ("convertVal: " ++ show x)
    cv (ValPrim (APrim p _) [x,y] _) = do
        x' <- convertVal x
        y' <- convertVal y
        case p of
            Op (Op.BinOp n ta tb) r -> primBinOp n ta tb r x' y'
            x -> return $ err ("convertVal: " ++ show x)

    cv x = return $ err ("convertVal: " ++ show x)

convertTypes [] = return voidType
convertTypes [t] = convertType t
convertTypes xs = do
    xs <- mapM convertType xs
    return (anonStructType xs)

convertType TyNode = return wptr_t
convertType (TyPtr TyNode) = return sptr_t
convertType (TyPtr (TyPtr TyNode)) = return $ ptrType sptr_t
convertType ~(TyPrim opty) = return (opTyToC opty)

tyToC _ Op.TyBool = "bool"
tyToC dh (Op.TyBits (Op.BitsExt s) _) = s
tyToC dh (Op.TyBits b h) = f b h where
    f b Op.HintNone = f b dh
    f b Op.HintUnsigned = case b of
        (Op.Bits n) ->  "uint" ++ show n ++ "_t"
        (Op.BitsArch Op.BitsMax) -> "uintmax_t"
        (Op.BitsArch Op.BitsPtr) -> "uintptr_t"
        _ -> error "tyToC: unknown"
    f b Op.HintSigned = case b of
        (Op.Bits n) ->  "int" ++ show n ++ "_t"
        (Op.BitsArch Op.BitsMax) -> "intmax_t"
        (Op.BitsArch Op.BitsPtr) -> "intptr_t"
        _ -> error "tyToC: unknown"
    f b Op.HintFloat = case b of
        (Op.Bits 32) -> "float"
        (Op.Bits 64) -> "double"
        (Op.Bits 128) -> "__float128"
        _ -> error "tyToC: unknown"
    f _ _ = error "tyToC: unknown"


opTyToCh hint opty = basicType (tyToC hint opty)
opTyToC opty = basicType (tyToC Op.HintUnsigned opty)
opTyToC' opty = tyToC Op.HintUnsigned opty

localScope xs action = do
    let fvs = freeVars xs
    aas <- mapM (\ (v,t) -> do t <- convertType t ; return . toStatement $ localVariable t (varName v)) (filter ((v0 /=) . fst) $ Set.toList fvs)
    local (rInscope_u $ Set.union (Set.map varName (freeVars xs))) (action . statementOOB $ mconcat aas)

iDeclare action = local (\e -> e { rDeclare = True }) action

convertBody :: Exp -> C Statement
{-convertBody Let { expDefs = defs, expBody = body } = do
    u <- newUniq
    nn <- flip mapM defs $ \FuncDef { funcDefName = name, funcDefBody = as :-> _ } -> do
        vs' <- mapM convertVal as
        let nm = (toName (show name ++ "_" ++ show u))
        return (as,(name,(nm,vs')))
    let done = (toName $ "done" ++ show u)
    let localJumps xs action = localScope (fsts xs) $ \dcls ->  local (rEMap_u (Map.fromList (snds xs) `mappend`)) (fmap (dcls &) action)
    localJumps nn $ do
    rs <- flip mapM defs $ \FuncDef { funcDefName = name, funcDefBody = as :-> b } -> do
        ss <- convertBody b
        return (annotate (show as) (label (toName (show name ++ "_" ++ show u))) & subBlock ss)
    ss <- (convertBody body)
    todo <- asks rTodo
    case todo of
        TodoReturn -> return (ss & mconcat rs);
        _ -> return (ss & goto done & mconcat (intersperse (goto done) rs) & label done);-}
convertBody (e :>>= [] :-> e') = do
    ss <- localTodo TodoNothing (convertBody e)
    ss' <- convertBody e'
    return (ss & ss')
convertBody (Return [v] :>>= [(NodeC t as)] :-> e') = nodeAssign v t as e'
convertBody (Fetch v :>>= [(NodeC t as)] :-> e') = nodeAssign v t as e'
convertBody (Case v [p1@([NodeC _ (_:_)] :-> _),p2@([NodeC _ []] :-> _)]) = convertBody $ Case v [p2,p1]
convertBody (Case v@(Var _ ty) [[p1@(NodeC t fps)] :-> e1,[p2] :-> e2]) | ty == TyNode = do
    scrut <- convertVal v
    cpr <- asks rCPR
    tellTags t
    let da (Var v _) e | v == v0 = convertBody e
        da v@Var {} e = do
            v'' <- iDeclare $ convertVal v
            e' <- convertBody e
            return $ v'' =* scrut & e'
        da n1@(NodeC t _) (Return [n2@NodeC {}]) | n1 == n2 = convertBody (Return [v])
        da ~(NodeC t as) e = nodeAssign v t as e
        {-
        da (NodeC t [a]) e | t `Set.member` cpr = do
            a' <- iDeclare $ convertVal a
            let tmp = concrete t  scrut
                ass = mconcat [if needed a then a' =* (project' (arg i) tmp) else mempty | a' <- as' | a <- as | i <- [(1 :: Int) ..] ]
                fve = freeVars e
                needed ~(Var v _) = v `Set.member` fve
            e' <- convertBody e
            return (ass & e')
        da ~(NodeC t as) e = do
            tellTags t
            declareStruct t
            as' <- iDeclare $ mapM convertVal as
            let tmp = concrete t  scrut
                ass = mconcat [if needed a then a' =* (project' (arg i) tmp) else mempty | a' <- as' | a <- as | i <- [(1 :: Int) ..] ]
                fve = freeVars e
                needed ~(Var v _) = v `Set.member` fve
            e' <- convertBody e
            return (ass & e')
            -}
        am Var {} e = e
        am ~(NodeC t2 _) e = annotate (show p2) (f_assert ((constant $ enum (nodeTagName t2)) `eq` tag) & e)
        tag = f_GETWHAT scrut
        ifscrut = if null fps then f_RAWWHAT tenum `eq` scrut else tenum `eq` tag where
            tenum = (constant $ enum (nodeTagName t))
    p1' <- da p1 e1
    p2' <- liftM (am p2) $ da p2 e2
    return $ profile_case_inc & cif ifscrut p1' p2'

-- zero is usually faster to test for than other values, so flip them if zero is being tested for.
convertBody (Case v@Var {} [v1, v2@([Lit n _] :-> _)]) | n == 0 = convertBody (Case v [v2,v1])
convertBody (Case v@(Var _ t) [[p1] :-> e1, [p2] :-> e2]) | Set.null ((freeVars p2 :: Set.Set Var) `Set.intersection` freeVars e2) = do
    scrut <- convertVal v
    let cp ~(Lit i _) = constant (number $ fromIntegral i)
        am e | isVar p2 = e
             | otherwise = annotate (show p2) (f_assert ((cp p2) `eq` scrut) & e)
    e1' <- convertBody e1
    e2' <- convertBody e2
    return $ profile_case_inc & cif (cp p1 `eq` scrut) e1' (am e2')
convertBody (Case v@(Var _ t) ls) | t == TyNode = do
    scrut <- convertVal v
    let tag = f_GETWHAT scrut
        da ([(Var v _)] :-> e) | v == v0 = do
            e' <- convertBody e
            return $ (Nothing,e')
        da ([v@(Var {})] :-> e) = do
            v'' <- iDeclare $ convertVal v
            e' <- convertBody e
            return $ (Nothing,v'' =* scrut & e')
        da ([n1@(NodeC t _)] :-> Return [n2@NodeC {}]) | n1 == n2 = do
            tellTags t
            e' <- convertBody (Return [v])
            return (Just (enum (nodeTagName t)),e')
        da (~[(NodeC t as)] :-> e) = do
            tellTags t
            declareStruct t
            as' <- iDeclare $ mapM convertVal as
            e' <- convertBody e
            let tmp = concrete t scrut
                ass = mconcat [if needed a then a' =* (project' (arg i) tmp) else mempty | a' <- as' | a <- as | i <- [(1 :: Int) ..] ]
                fve = freeVars e
                needed ~(Var v _) = v `Set.member` fve
            return $ (Just (enum (nodeTagName t)), ass & e')
    ls' <- mapM da ls
    return $ profile_case_inc & switch' tag ls'
convertBody (Case v@(Var _ t) ls) = do
    scrut <- convertVal v
    let da ([(Var vv _)] :-> e) | vv == v0 = do
            e' <- convertBody e
            return (Nothing,e')
        da ([v@(Var {})] :-> e) = do
            v'' <- iDeclare $ convertVal v
            e' <- convertBody e
            return (Nothing,v'' =* scrut & e')
        da (~[(Lit i _)] :-> e) = do
            e' <- convertBody e
            return $ (Just (number $ fromIntegral i), e')
        --da (~[x] :-> e) = da ( x :-> e )
    ls' <- mapM da ls
    return $ profile_case_inc & switch' scrut ls'
convertBody (Error s t) = do
    x <- asks rTodo
    let jerr | null s    = toStatement $ functionCall (name "lhc_exit") [constant $ number 255]
             | otherwise = toStatement $ functionCall (name "lhc_error") [string s]
    let f (TyPtr _) = return nullPtr
        f TyNode = return nullPtr
        f (TyPrim x) = return $ cast (opTyToC x) (constant $ number 0)
        f x = return $ err ("error-type " ++ show x)
        g [] = return emptyExpression
        g [x] = f x
        g xs = do ts <- mapM convertType xs; xs <- mapM f xs ; return $ structAnon (zip xs ts)
    case x of
        TodoNothing -> return jerr
        TodoExp _ -> return jerr
        TodoDecl {} -> return jerr
        TodoReturn -> do
            v <- g t
            return (jerr & creturn v)

convertBody (Store  n@NodeC {})  = newNode sptr_t n >>= \(x,y) -> simpleRet y >>= \v -> return (x & v)
convertBody (Return [n@NodeC {}])  = newNode wptr_t n >>= \(x,y) -> simpleRet y >>= \v -> return (x & v)


convertBody (e :>>= [(Var vn _)] :-> e') | vn == v0 = do
    ss <- localTodo TodoNothing (convertBody e)
    ss' <- convertBody e'
    return (ss & ss')

convertBody (e :>>= [(Var vn' vt')] :-> e') | not (isCompound e) = do
    (vn,vt) <- fetchVar' vn' vt'
    ss <- localTodo (TodoDecl vn vt) (convertBody e)
    ss' <- convertBody e'
    return (ss & ss')

convertBody (e :>>= [v@(Var vn vt)] :-> e') = do
    v' <- convertVal v
    vt <- convertType vt
    let sdecl = statementOOB $ toStatement (localVariable vt (varName vn))
    ss <- localTodo (TodoExp [v'])  (convertBody e)
    ss' <- convertBody e'
    return (sdecl & ss & ss')

convertBody (e :>>= xs@(_:_:_) :-> e') = do
    ts <- mapM (convertType . getType) xs
    (dcl,st) <- newDeclVar (anonStructType ts)
    vs <- iDeclare $ mapM convertVal xs
    ss <- localTodo (TodoExp [st]) (convertBody e)
    ss' <- convertBody e'
    return $ dcl & ss & mconcat [ v =* projectAnon i st | v <- vs | i <- [0..] ] & ss'

-- mutable arrays and iorefs
convertBody (Update (Index base off) z) | getType base == TyPtr tyINode = do
    base <- convertVal base
    off <- convertVal off
    z' <- convertVal z
    return $ indexArray base off =* z'
convertBody (Fetch (Index base off)) | getType base == TyPtr tyINode = do
    base <- convertVal base
    off <- convertVal off
    simpleRet (indexArray base off)

-- return, promote and demote
convertBody (Fetch v)        | getType v == tyINode = simpleRet =<< f_promote `liftM` convertVal v
convertBody (Store n@Var {}) | getType n == tyDNode = simpleRet =<< f_demote `liftM` convertVal n

convertBody (Return []) = simpleRet emptyExpression
convertBody (Return [v]) = simpleRet =<< convertVal v
convertBody (Return xs@(_:_:_)) = do
    t <- asks rTodo
    case t of
        TodoExp [e] -> do
            xs <- mapM convertVal xs
            ss <- forMn xs $ \ (v,i) -> return (projectAnon i e =* v)
            return (mconcat ss)
        _ -> simpleRet =<< convertVals xs


convertBody e = do
    x <- asks rTodo
    (ss,er) <- convertExp e
    r <- simpleRet er
    return (ss & r)


simpleRet er = do
    x <- asks rTodo
    case x of
        TodoReturn -> return (creturn er)
        _ | isEmptyExpression er -> return mempty
        TodoNothing -> return (toStatement er)
        TodoExp [v] -> return (v =* er)
        TodoDecl n t -> do newAssignVar t n er
        TodoExp [] -> return $ toStatement er
        _ -> error "simpleRet: odd rTodo"

nodeAssign :: Val -> Atom -> [Val] -> Exp -> C Statement
nodeAssign v t as e' = cna where
    cna = do
        cpr <- asks rCPR
        if t `Set.notMember` cpr then na else do
        v' <- convertVal v
        [arg] <- return as
        t <- convertType $ getType arg
        arg' <- iDeclare $ convertVal arg
        let s = arg' =* cast t (f_GETVALUE v')
        ss <- convertBody e'
        return $ s & ss
    na = do
        declareStruct t
        v' <- convertVal v
        as' <- iDeclare $ mapM convertVal as
        let ass = concat [perhapsM (a `Set.member` fve) $ a' =* (project' (arg i) (concrete t v')) | a' <- as' | Var a _ <- as |  i <- [( 1 :: Int) ..] ]
            fve = freeVars e'
        ss' <- convertBody e'
        return $ mconcat ass & ss'

isCompound Fetch {} = False
isCompound Return {} = False
isCompound Store {} = False
isCompound Prim {} = False
isCompound _ = True


convertExp :: Exp -> C (Statement,Expression)
convertExp (Prim p vs ty) | APrim _ req <- p  =  do
    tell mempty { wRequires = req }
    e <- convertPrim p vs ty
    return (mempty,e)


--convertExp (App a [fn,x] _) | a == funcApply = do
--    fn' <- convertVal fn
--    x' <- convertVal x
--    return (mempty,(functionCall (name "eval") [v']))
convertExp (App a [v] _) | a == funcEval = do
    v' <- convertVal v
    return (mempty,f_eval v')
convertExp (App a vs _) = do
    lm <- asks rEMap
    vs' <- mapM convertVal vs
    case a `Map.lookup` lm of
        Just (nm,as) -> do
            let ss = [ a =* v | a <- as | v <- vs' ]
            return (mconcat ss & goto nm, emptyExpression)
        Nothing -> return $ (mempty, functionCall (toName (fromAtom a)) vs')
convertExp (Update v@(Var vv _) tn@(NodeC t as)) | getType v == TyPtr TyNode = do
    v' <- convertVal v
    as' <- mapM convertVal as
    nt <- nodeTypePtr t
    let tmp' = cast nt (f_DETAG v') -- (if vv < v0 then f_DETAG v' else v')
    if not (tagIsSuspFunction t) && vv < v0 then do
        (nns, nn) <- newNode fptr_t tn
        return (nns & getHead (f_NODEP(f_DETAG v')) =* nn,emptyExpression)
     else do
        s <- tagAssign tmp' t
        let ass = [project' (arg i) tmp' =* a | a <- as' | i <- [(1 :: Int) ..] ]
        return (mconcat $ profile_update_inc:s:ass,emptyExpression)

convertExp Alloc { expValue = v, expCount = c, expRegion = r } | r == region_heap, TyPtr TyNode == getType v  = do
    v' <- convertVal v
    c' <- convertVal c
    tmp <- newVar (ptrType sptr_t)
    let malloc = tmp =* lhc_malloc (operator "*" (sizeof sptr_t) c')
    fill <- case v of
        ValUnknown _ -> return mempty
        _ -> do
            i <- newVar (basicType "int")
            return $ forLoop i (expressionRaw "0") c' $ indexArray tmp i =* v'
    return (malloc `mappend` fill, tmp)

convertExp e = return (err (show e),err "nothing")

ccaf :: (Var,Val) -> P.Doc
ccaf (v,val) = text "/* " <> text (show v) <> text " = " <> (text $ show (pprint val :: P.Doc)) <> text "*/\n" <>
     text "static node_t _" <> tshow (varName v) <> text ";\n" <>
     text "#define " <> tshow (varName v) <+>  text "(EVALTAGC(&_" <> tshow (varName v) <> text "))\n";


buildConstants grin fh = P.vcat (map cc (Grin.HashConst.toList fh)) where
    tyenv = grinTypeEnv grin
    comm nn = text "/* " <> tshow (nn) <> text " */"
    cc nn@(HcNode a zs,i) = comm nn <$$> cd <$$> def where
        cd = text "static const struct" <+> tshow (nodeStructName a) <+> text "_c" <> tshow i <+> text "= {" <> hsep (punctuate P.comma (ntag ++ rs)) <> text "};"
        Just TyTy { tySiblings = sibs } = findTyTy tyenv a
        ntag = case sibs of
            Just [a'] | a' == a -> []
            _ -> [text ".what =" <+> tshow (nodeTagName a)]
        def = text "#define c" <> tshow i <+> text "((sptr_t)&_c" <> tshow i <> text ")"
        rs = [ f z i |  (z,i) <- zip zs [ 1 :: Int .. ]]
        f (Right i) a = text ".a" <> tshow a <+> text "=" <+> text ('c':show i)
        f (Left (Var n _)) a = text ".a" <> tshow a <+> text "=" <+> tshow (varName n)
        f (Left v) a = text ".a" <> tshow a <+> text "=" <+> text (show $ drawG e) where
            Just e = fst3 . runC grin $ convertConst v

convertConst :: Val -> C (Maybe Expression)
convertConst (NodeC n as) | all valIsConstant as = basicNode n as
convertConst (Const (NodeC n as)) = fmap (fmap $ cast sptr_t) $ basicNode n as
convertConst v = return (f v) where
    f :: Val -> Maybe Expression
    f (Lit i (TyPrim Op.TyBool)) = return $ if i == 0 then constant cFalse else constant cTrue
    f (Lit i (TyPrim (Op.TyBits _ Op.HintFloat))) = return (constant $ floating (realToFrac i))
    f (Lit i _) = return (constant $ number (fromIntegral i))
    f (ValPrim (APrim p _) [] ty) = case p of
        CConst s _ -> return $ expressionRaw s
        AddrOf t -> do rt <- convertType ty; return . cast rt $ expressionRaw ('&':unpackPS t)
        PrimTypeInfo { primArgTy = arg, primTypeInfo = PrimSizeOf } -> return $ expressionRaw ("sizeof(" ++ tyToC Op.HintUnsigned arg ++ ")")
        PrimTypeInfo { primArgTy = arg, primTypeInfo = PrimMinBound } -> return $ expressionRaw ("prim_minbound(" ++ tyToC Op.HintUnsigned arg ++ ")")
        PrimTypeInfo { primArgTy = arg, primTypeInfo = PrimMaxBound } -> return $ expressionRaw ("prim_maxbound(" ++ tyToC Op.HintUnsigned arg ++ ")")
        PrimTypeInfo { primArgTy = arg, primTypeInfo = PrimUMaxBound } -> return $ expressionRaw ("prim_umaxbound(" ++ tyToC Op.HintUnsigned arg ++ ")")
        PrimString s -> return $ cast (basicType "uintptr_t") (expressionRaw (show s))
        x -> return $ err (show x)
    f (ValPrim (APrim p _) [x] (TyPrim opty)) = do
        x' <- f x
        case p of
            Op (Op.UnOp n ta) r -> primUnOp n ta r x'
            Op (Op.ConvOp n ta) r -> return $ castFunc n ta r x'
            x -> return $ err (show x)
    f (ValPrim (APrim p _) [x,y] _) = do
        x' <- f x
        y' <- f y
        case p of
            Op (Op.BinOp n ta tb) r -> primBinOp n ta tb r x' y'
            x -> return $ err (show x)
    f x = fail "f"


--convertPrim p vs = return (mempty,err $ show p)
convertPrim p vs ty
    | APrim (CConst s _) _ <- p = do
        return $ expressionRaw s
    | APrim Op {} _ <- p = do
        let [rt] = ty
        convertVal (ValPrim (p) vs rt)
    | APrim (Func _ n as r) _ <- p = do
        vs' <- mapM convertVal vs
        rt <- convertTypes ty
        return $ cast (rt) (functionCall (name $ unpackPS n) [ cast (basicType t) v | v <- vs' | t <- as ])
    | APrim (IFunc _ as r) _ <- p = do
        v':vs' <- mapM convertVal vs
        rt <- convertTypes ty
        let fn = cast (funPtrType (basicType r) (map basicType as)) v'
        return $ cast (rt) (indirectFunctionCall fn [ cast (basicType t) v | v <- vs' | t <- as ])
    | APrim (Peek t) _ <- p, [v] <- vs = do
        v' <- convertVal v
        return $ expressionRaw ("*((" <> (opTyToC' t) <+> "*)" <> (parens $ renderG v') <> char ')')
    | APrim (Poke t) _ <- p, [v,x] <- vs = do
        v' <- convertVal v
        x' <- convertVal x
        return $ expressionRaw ("*((" <> (opTyToC' t) <+> "*)" <> (parens $ renderG v') <> text ") = " <> renderG x')
    | APrim (AddrOf t) _ <- p, [] <- vs = do
        rt <- convertTypes ty
        return . cast rt $ expressionRaw ('&':unpackPS t)
    | otherwise = return $ err ("prim: " ++ show (p,vs))

signedOps = [
    (Op.Div,"/"),  -- TODO round to -Infinity
    (Op.Mod,"%"),  -- TODO round to -Infinity
    (Op.Quot,"/"),
    (Op.Rem,"%"),
    (Op.Shra,">>"),
    (Op.Gt,">"),
    (Op.Lt,"<"),
    (Op.Gte,">="),
    (Op.Lte,"<=")
    ]

floatOps = [
    (Op.FDiv,"/"),
    (Op.FAdd,"+"),
    (Op.FSub,"-"),
    (Op.FMul,"*"),
    (Op.FEq,"=="),
    (Op.FNEq,"!="),
    (Op.FGt,">"),
    (Op.FLt,"<"),
    (Op.FGte,">="),
    (Op.FLte,"<=")
    ]


binopSigned :: Op.BinOp -> Maybe String
binopSigned b = lookup b signedOps

castSigned ty v = return $ cast (basicType $ tyToC Op.HintSigned ty) v

primBinOp n ta tb r a b
    | Just fn <- Op.binopFunc ta tb n = return $ functionCall (toName fn) [a,b]
    | Just (t,_) <- Op.binopInfix n = return $ operator t a b
    | Just t <- binopSigned n = do
        a <- castSigned ta a
        b <- castSigned tb b
        return $ operator t a b
    | Just t <- lookup n floatOps = return $ operator t a b
    | otherwise = return $ err ("primBinOp: " ++ show ((n,ta,tb,r),a,b))

primUnOp Op.Neg ta r a = do
    a <- castSigned ta a
    return $ uoperator "-" a
primUnOp Op.Com ta r a = do return $ uoperator "~" a
primUnOp Op.FNeg ta r a = do return $ uoperator "-" a
primUnOp op ta r a | Just fn <- Op.unopFloat ta op = return $ functionCall (toName fn) [a]
primUnOp n ta r a
    | otherwise = return $ err ("primUnOp: " ++ show ((n,ta,r),a))


tagAssign :: Expression -> Atom -> C Statement
tagAssign e t | tagIsSuspFunction t = do
    en <- declareEvalFunc t
    return $ getHead e =* f_EVALFUNC (reference (variable en))
tagAssign e t = do
    declareStruct t
    tyenv <- asks (grinTypeEnv . rGrin)
    TyTy { tySiblings = sib } <- findTyTy tyenv t
    tellTags t
    case sib of
        Just [n'] | n' == t -> return mempty
        _ -> do return . toStatement $ f_SETWHAT e (constant (enum $ nodeTagName t))


tellAllTags :: Val -> C ()
tellAllTags (NodeC n vs) = tellTags n >> mapM_ tellAllTags vs
tellAllTags n = mapValVal tt n >> return () where
    tt v = tellAllTags v >> return v

tellTags :: Atom -> C ()
tellTags t | tagIsSuspFunction t = return ()
tellTags t = do
    tyenv <- asks (grinTypeEnv . rGrin)
    TyTy { tySiblings = sib } <- findTyTy tyenv t
    case sib of
--        Just [n'] | n' == t ->  return ()
        Just rs -> tell mempty { wEnums = Map.fromList (zip (map nodeTagName rs) [0..]) }
        Nothing -> tell mempty { wTags = Set.singleton t }



newNode ty ~(NodeC t as) = do
    let sf = tagIsSuspFunction t
    bn <- basicNode t as
    case bn of
      Just e -> return (mempty,if ty == wptr_t then e else cast ty e)
      Nothing -> do
        st <- nodeType t
        as' <- mapM convertVal as
        let wmalloc = if not sf && all (nonPtr . getType) as then lhc_malloc_atomic else lhc_malloc
            malloc =  wmalloc (sizeof st)
            nonPtr TyPtr {} = False
            nonPtr TyNode = False
--            nonPtr (TyTup xs) = all nonPtr xs
            nonPtr _ = True
        (dtmp,tmp) <- ty `newTmpVar` malloc
        let tmp' = concrete t tmp
            ass = [ if isValUnknown aa then mempty else project' i tmp' =* a | a <- as' | aa <- as | i <- map arg [(1 :: Int) ..] ]
        tagassign <- tagAssign tmp' t
        let res = if sf then (f_EVALTAG tmp) else tmp
        return (mconcat $ dtmp:tagassign:ass,res)



------------------
-- declaring stuff
------------------

declareStruct n = do
    grin <- asks rGrin
    let TyTy { tySlots = ts, tySiblings = ss } = runIdentity $ findTyTy (grinTypeEnv grin) n
    ts' <- mapM convertType ts
    let (dis,needsDis) | tagIsSuspFunction n = ([(name "head",fptr_t)],False)
                       | null ts = ([],False)
                       | Just [n'] <- ss, n == n' = ([],False)
                       | otherwise = ([],True)
        fields = (dis ++ zip [ name $ 'a':show i | i <-  [(1 :: Int) ..] ] ts')
        theStruct = basicStructure {
            structureName = nodeStructName n,
            structureFields = fields,
            structureAligned = True,
            structureHasDiscriminator = not $ null dis,
            structureNeedsDiscriminator = needsDis
            }
    unless (null fields) $ tell mempty { wStructures = Map.singleton (structureName theStruct) theStruct }


basicNode :: Atom -> [Val] -> C (Maybe Expression)
basicNode a _ | tagIsSuspFunction a = return Nothing
basicNode a []  = do tellTags a ; return . Just $ (f_RAWWHAT (constant $ enum (nodeTagName a)))
basicNode a [v] = do
    cpr <- asks rCPR
    if a `Set.notMember` cpr then return Nothing else
        case v of
            Lit i ty | a == cChar, Just c <- ch -> return $ Just (f_VALUE (toExpression c)) where
                ch = do
                    c <- toIntegral i
                    guard $ c >= ord minBound && c <= ord maxBound
                    c <- return $ chr c
                    guard $ isPrint c && isAscii c
                    return c
            _ -> do
                v <- convertVal v
                return $ Just (f_VALUE v)
basicNode _ _ = return Nothing

instance Op.ToCmmTy Ty where
    toCmmTy (TyPrim p) = Just p
    toCmmTy _ = Nothing

declareEvalFunc n = do
    fn <- tagToFunction n
    grin <- asks rGrin
    declareStruct n
    nt <- nodeType n
    let ts = runIdentity $ findArgs (grinTypeEnv grin) n
        fname = toName $ "lhc_eval_" ++ show fn
        aname = name "arg";
        rvar = localVariable wptr_t (name "r");
        atype = ptrType nt
        body = rvar =* functionCall (toName (show $ fn)) [ project' (arg i) (variable aname) | _ <- ts | i <- [(1 :: Int) .. ] ]
        update =  f_update (cast sptr_t (variable aname)) rvar
    tellFunctions [function fname wptr_t [(aname,atype)] [a_STD, a_FALIGNED] (body & update & creturn rvar )]
    return fname


castFunc :: Op.ConvOp -> Op.Ty -> Op.Ty -> Expression -> Expression
castFunc co ta tb e | ta == tb = e
castFunc co _ Op.TyBool e = cast (basicType "bool") e
castFunc co Op.TyBool tb e = cast (opTyToC tb) e

castFunc Op.Lobits _ tb e = cast (opTyToC tb) e
castFunc Op.U2U _ tb e = cast (opTyToC tb) e
castFunc Op.Zx _ tb e = cast (opTyToC tb) e

castFunc Op.I2I tf tb e = cast (opTyToCh Op.HintSigned tb) (cast (opTyToCh Op.HintSigned tf) e)
castFunc Op.Sx tf tb e = cast (opTyToCh Op.HintSigned tb) (cast (opTyToCh Op.HintSigned tf) e)

castFunc Op.F2I tf tb e = cast (opTyToCh Op.HintSigned tb) e
castFunc Op.I2F tf tb e = cast (opTyToC tb) (cast (opTyToCh Op.HintSigned tf) e)

castFunc _ _ tb e = cast (opTyToC tb) e



----------------------------
-- c constants and utilities
----------------------------

lhc_malloc sz = functionCall (name "lhc_malloc") [sz]
f_assert e    = functionCall (name "assert") [e]
f_DETAG e     = functionCall (name "DETAG") [e]
f_NODEP e     = functionCall (name "NODEP") [e]
f_VALUE e     = functionCall (name "VALUE") [e]
f_GETVALUE e  = functionCall (name "GETVALUE") [e]
f_EVALTAG e   = functionCall (name "EVALTAG") [e]
f_EVALFUNC e  = functionCall (name "EVALFUNC") [e]
f_eval e      = functionCall (name "eval") [e]
f_promote e   = functionCall (name "promote") [e]
f_PROMOTE e   = functionCall (name "PROMOTE") [e]
f_GETWHAT e   = functionCall (name "GETWHAT") [e]
f_SETWHAT e v = functionCall (name "SETWHAT") [e,v]
f_RAWWHAT e   = functionCall (name "RAWWHAT") [e]
f_demote e    = functionCall (name "demote") [e]
f_follow e    = functionCall (name "follow") [e]
f_update x y  = functionCall (name "update") [x,y]
lhc_malloc_atomic sz = functionCall (name "lhc_malloc_atomic") [sz]
profile_update_inc   = toStatement $ functionCall (name "lhc_update_inc") []
profile_case_inc     = toStatement $ functionCall (name "lhc_case_inc") []
profile_function_inc = toStatement $ functionCall (name "lhc_function_inc") []

arg i = name $ 'a':show i


varName (V n) | n < 0 = name $ 'g':show (- n)
varName (V n) = name $ 'v':show n

nodeTagName :: Atom -> Name
nodeTagName a = toName (fromAtom a)
nodeFuncName :: Atom -> Name
nodeFuncName a = toName (fromAtom a)

sptr_t    = basicGCType "sptr_t"
fptr_t    = basicGCType "fptr_t"
wptr_t    = basicGCType "wptr_t"

a_STD = Attribute "A_STD"
a_FALIGNED = Attribute "A_FALIGNED"
a_MALLOC = Attribute "A_MALLOC"

concrete :: Atom -> Expression -> Expression
concrete t e = cast (ptrType $ structType (nodeStructName t)) e


getHead :: Expression -> Expression
getHead e = project' (name "head") e

nodeTypePtr a = liftM ptrType (nodeType a)
nodeType a = return $ structType (nodeStructName a)
nodeStructName :: Atom -> Name
nodeStructName a = toName ('s':fromAtom a)


generateArchAssertions :: String
generateArchAssertions = unlines (h:map f (filter notVoid as) ++ [t]) where
    (_,as,_) = unsafePerformIO determineArch
    notVoid pt = primTypeName pt /= "void"
    f pt = printf "      assert(sizeof(%s) == %d);" (primTypeName pt) (primTypeSizeOf pt)
    h = "static void\nlhc_arch_assert(void)\n{"
    t = "}"




