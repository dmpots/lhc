module FrontEnd.Tc.Monad(
    CoerceTerm(..),
    Tc(),
    MonadTc(..),
    asksTc,
    censorTc,
    TcInfo(..),
    TypeEnv(),
    TcEnv(..),
    tcRecursiveCalls_u,
    Output(..),
    addCoerce,
    addPreds,
    composeCoerce,
    addRule,
    addToCollectedEnv,
    boxyInstantiate,
    boxySpec,
    deconstructorInstantiate,
    freeMetaVarsEnv,
    freshInstance,
    freshSigma,
    getClassHierarchy,
    getCollectedEnv,
    getCollectedCoerce,
    getKindEnv,
    getModName,
    getSigEnv,
    evalFullType,
    inst,
    listenCheckedRules,
    listenPreds,
    listenCPreds,
    localEnv,
    lookupName,
    newBox,
    newMetaVar,
    newVar,
    quantify,
    runTc,
    skolomize,
    tcInfoEmpty,
    toSigma,
    unBox,
    evalType,
    unificationError,
    varBind,
    zonkKind,
    withContext,
    withMetaVars
    ) where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Traversable as T

import Data.DeriveTH
import Data.Derive.All
import Data.IORef
import Data.Monoid
import List
import Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import System


import StringTable.Atom
import FrontEnd.Diagnostic
import Doc.DocLike
import Doc.PPrint
import FrontEnd.Class
import FrontEnd.KindInfer
import FrontEnd.SrcLoc(bogusASrcLoc,MonadSrcLoc(..))
import FrontEnd.Tc.Type
import FrontEnd.Tc.Kind
import GenUtil
import Name.Name
import Name.Names
import Options
import Support.CanType
import Support.FreeVars
import Support.Tickle
import FrontEnd.Warning
import Util.ContextMonad
import qualified FlagDump as FD
import {-# SOURCE #-} FrontEnd.Tc.Class(ClassHierarchy,simplify)


data BindingType = RecursiveInfered | Supplied
type TypeEnv = Map.Map Name Sigma

-- | Read only environment, set up before type checking.
data TcEnv = TcEnv {
    tcInfo              :: TcInfo,
    tcDiagnostics       :: [Diagnostic],       -- ^ Context for use in type error messages
    tcVarnum            :: IORef Int,
    tcCollectedEnv      :: IORef (Map.Map Name Sigma),
    tcCollectedCoerce   :: IORef (Map.Map Name CoerceTerm),
    tcCurrentEnv        :: Map.Map Name Sigma,
    tcCurrentScope      :: Set.Set MetaVar,
    tcRecursiveCalls    :: Set.Set Name,
    tcInstanceEnv       :: InstanceEnv,
    tcOptions           :: Opt                 -- ^ Module-specific options
    }

data Output = Output {
    collectedPreds   :: Preds,
    existentialPreds :: Preds,
    constraints      :: [Constraint],
    checkedRules     :: [Rule],
    existentialVars  :: [Tyvar],
    outKnots         :: [(Name,Name)]
    }

-- | Information that is passed into the type checker.
data TcInfo = TcInfo {
    tcInfoEnv :: TypeEnv,      -- ^ Initial typeenv, data constructors, and previously infered types
    tcInfoSigEnv :: TypeEnv,   -- ^ Type signatures used for binding analysis
    tcInfoModName :: String,
    tcInfoKindInfo :: KindEnv,
    tcInfoClassHierarchy :: ClassHierarchy
    }

$(derive makeUpdate ''TcEnv)
$(derive makeUpdate ''Output)
$(derive makeMonoid ''Output)

newtype Tc a = Tc (ReaderT TcEnv (WriterT Output IO) a)
    deriving (MonadFix,MonadIO,Functor) -- ,MonadReader TcEnv,MonadWriter Output,Functor)

class MonadIO m => MonadTc m where
    askTc    :: m TcEnv
    localTc  :: (TcEnv -> TcEnv) -> m a -> m a

    tellTc   :: Output -> m ()
    listenTc :: m a -> m (a, Output)
    passTc   :: m (a, Output -> Output) -> m a

asksTc :: MonadTc m => (TcEnv -> a) -> m a
asksTc f = liftM f askTc

listensTc :: MonadTc m => (Output -> b) -> m a -> m (a, b)
listensTc f p = do (x, o) <- listenTc p
                   return (x, f o)

censorTc :: MonadTc m => (Output -> Output) -> m a -> m a
censorTc f p = passTc $ do x <- p
                           return (x, f)
                                

instance MonadTc Tc where
    askTc            = Tc ask
    localTc f (Tc p) = Tc (local f p)
    
    tellTc x         = Tc (tell x)
    listenTc (Tc p)  = Tc (listen p)
    passTc   (Tc p)  = Tc (pass p)


-- | Run a computation with a local environment
localEnv :: TypeEnv -> Tc a -> Tc a
localEnv te act = do
    te' <- mapM (\ (x,y) -> do y <- flattenType y; return (x,y)) (Map.toList te)
    if any isBoxy (snds te') then
        fail $ "localEnv error!\n" ++ show te
     else localTc (tcCurrentEnv_u (Map.fromList te' `Map.union`)) act

-- | Add to the collected environment which will be used to annotate uses of variables with their instantiated types.
-- Should contain \@-aliases for each use of a polymorphic variable or pattern match.

addToCollectedEnv :: TypeEnv -> Tc ()
addToCollectedEnv te = do
    v <- asksTc tcCollectedEnv
    liftIO $ modifyIORef v (te `Map.union`)

addCoerce :: Name -> CoerceTerm -> Tc ()
addCoerce n te = do
    v <- asksTc tcCollectedCoerce
    liftIO $ modifyIORef v (Map.insert n te)

getCollectedEnv :: Tc TypeEnv
getCollectedEnv = do
    v <- asksTc tcCollectedEnv
    r <- liftIO $ readIORef v
    r <- T.mapM flattenType r
    return r

getCollectedCoerce :: Tc (Map.Map Name CoerceTerm)
getCollectedCoerce = do
    v <- asksTc tcCollectedCoerce
    r <- liftIO $ readIORef v
    r <- T.mapM flattenType r
    return r


runTc :: (MonadIO m,OptionMonad m) => TcInfo -> Tc a -> m a
runTc tcInfo  (Tc tim) = do
    opt <- getOptions
    liftIO $ do
    vn <- newIORef 0
    ce <- newIORef mempty
    cc <- newIORef mempty
    (a,out) <- runWriterT $ runReaderT tim TcEnv {
        tcCollectedEnv = ce,
        tcCollectedCoerce = cc,
        tcCurrentEnv = tcInfoEnv tcInfo `mappend` tcInfoSigEnv tcInfo,
        tcVarnum = vn,
        tcDiagnostics = [Msg Nothing $ "Compilation of module: " ++ tcInfoModName tcInfo],
        tcInfo = tcInfo,
        tcRecursiveCalls = mempty,
        tcInstanceEnv = makeInstanceEnv (tcInfoClassHierarchy tcInfo),
        tcCurrentScope = mempty,
        tcOptions = opt
        }
    return a

instance MonadTc m => OptionMonad m where
    getOptions = asksTc tcOptions

instance ContextMonad Diagnostic Tc where
    withContext diagnostic comp = do
        localTc (tcDiagnostics_u (diagnostic:)) comp

addRule :: Rule -> Tc ()
addRule r = tellTc mempty { checkedRules = [r] }


getErrorContext :: Tc [Diagnostic]
getErrorContext = asksTc tcDiagnostics

getClassHierarchy  :: Tc ClassHierarchy
getClassHierarchy = asksTc (tcInfoClassHierarchy . tcInfo)

getKindEnv :: Tc KindEnv
getKindEnv = asksTc (tcInfoKindInfo . tcInfo)

getSigEnv :: Tc TypeEnv
getSigEnv = asksTc (tcInfoSigEnv . tcInfo)

getModName :: Tc String
getModName = asksTc (tcInfoModName . tcInfo)



dConScheme :: Name -> Tc Sigma
dConScheme conName = do
    env <- asksTc tcCurrentEnv
    case Map.lookup conName env of
        Just s -> return s
        Nothing -> error $ "dConScheme: constructor not found: " ++ show conName ++
                              "\nin this environment:\n" ++ show env



-- | returns a new box and a function to read said box.

newBox :: Kind -> Tc Type
newBox k = newMetaVar Sigma k



unificationError :: Type -> Type -> Tc a
unificationError t1 t2 = do
    t1 <- evalFullType t1
    t2 <- evalFullType t2
    diagnosis <- getErrorContext
    let Left msg = typeError (Unification $ "attempted to unify " ++ prettyPrintType t1 ++ " with " ++ prettyPrintType t2) diagnosis
    liftIO $ processIOErrors
    liftIO $ putErrLn msg
    liftIO $ exitFailure


lookupName :: Name -> Tc Sigma
lookupName n = do
    env <- asksTc tcCurrentEnv
    case Map.lookup n env of
        Just x -> freshSigma x
        Nothing | Just 0 <- fromUnboxedNameTuple n  -> do
            return (tTTuple' [])
        Nothing | Just num <- fromUnboxedNameTuple n -> do
            nvs <- mapM newVar  (replicate num kindArg)
            let nvs' = map TVar nvs
            return (TForAll nvs $ [] :=> foldr TArrow  (tTTuple' nvs') nvs')
        Nothing -> fail $ "Could not find var in tcEnv:" <+> show (nameType n,n)
                          <$> "env:" <+> pprint env
                          

newMetaVar :: MetaVarType -> Kind -> Tc Type
newMetaVar t k = do
    te <- askTc
    n <- newUniq
    r <- liftIO $ newIORef Nothing
    return $ TMetaVar MetaVar { metaUniq = n, metaKind = k, metaRef = r, metaType = t }


class Instantiate a where
    inst:: Map.Map Int Type -> Map.Map Atom Type -> a -> a

instance Instantiate Type where
    inst mm ts (TAp l r)     = tAp (inst mm ts l) (inst mm ts r)
    inst mm ts (TArrow l r)  = TArrow (inst mm ts l) (inst mm ts r)
    inst mm  _ t@TCon {}     = t
    inst mm ts (TVar tv ) = case Map.lookup (tyvarAtom tv) ts of
            Just t'  -> t'
            Nothing -> (TVar tv)
    inst mm ts (TForAll as qt) = TForAll as (inst mm (foldr Map.delete ts (map tyvarAtom as)) qt)
    inst mm ts (TExists as qt) = TExists as (inst mm (foldr Map.delete ts (map tyvarAtom as)) qt)
    inst mm ts (TMetaVar mv) | Just t <- Map.lookup (metaUniq mv) mm  = t
    inst mm ts (TMetaVar mv) = TMetaVar mv
    inst mm ts (TAssoc tc as bs) = TAssoc tc (map (inst mm ts) as) (map (inst mm ts) bs)
--    inst mm _ t = error $ "inst: " ++ show t


instance Instantiate a => Instantiate [a] where
  inst mm ts = map (inst mm ts)

instance Instantiate t => Instantiate (Qual t) where
  inst mm ts (ps :=> t) = inst mm ts ps :=> inst mm ts t

instance Instantiate Pred where
  inst mm ts is = tickle (inst mm ts :: Type -> Type) is -- (IsIn c t) = IsIn c (inst mm ts t)


freshInstance :: MetaVarType -> Sigma -> Tc ([Type],Rho)
freshInstance typ (TForAll as qt) = do
    ts <- mapM (newMetaVar typ) (map tyvarKind as)
    let (ps :=> t) = (applyTyvarMapQT (zip as ts) qt)
    addPreds ps
    return (ts,t)
freshInstance _ x = return ([],x)

addPreds :: Preds -> Tc ()
addPreds ps = do
    sl <- getSrcLoc
    Tc $ tell mempty { collectedPreds = [ p | p@IsIn {} <- ps ],
                       constraints    = [ Equality { constraintSrcLoc = sl, constraintType1 = a, constraintType2 = b }
                                            | IsEq a b <- ps ] }

addConstraints :: [Constraint] -> Tc ()
addConstraints ps = Tc $ tell mempty { constraints = ps }

listenPreds :: Tc a -> Tc (a,Preds)
listenPreds action = censorTc (\x -> x { collectedPreds = mempty }) $ listensTc collectedPreds action

listenCPreds :: Tc a -> Tc (a,(Preds,[Constraint]))
listenCPreds action = censorTc (\x -> x { constraints = mempty, collectedPreds = mempty }) $
                        listensTc (\x -> (collectedPreds x,constraints x)) action

listenCheckedRules :: Tc a -> Tc (a,[Rule])
listenCheckedRules action = censorTc (\x -> x { checkedRules = mempty }) $ listensTc checkedRules action

newVar :: Kind -> Tc Tyvar
newVar k = do
    te <- askTc
    n <- newUniq
    let ident = toName TypeVal (tcInfoModName $ tcInfo te,'v':show n)
        v = tyvar ident k
    return v

-- rename the bound variables of a sigma, just in case.
freshSigma :: Sigma -> Tc Sigma
freshSigma (TForAll [] ([] :=> t)) = return t
freshSigma (TForAll vs qt) = do
    nvs <- mapM (newVar . tyvarKind) vs
    return (TForAll nvs $ applyTyvarMapQT (zip vs (map TVar nvs)) qt)
freshSigma x = return x

toSigma :: Sigma -> Sigma
toSigma t@TForAll {} = t
toSigma t = TForAll [] ([] :=> t)

-- | replace bound variables with arbitrary new ones and drop the binding.
-- TODO predicates?

skolomize :: Sigma -> Tc ([Tyvar],[Pred],Type)
skolomize s = freshSigma s >>= return . fromType

boxyInstantiate :: Sigma -> Tc ([Type],Rho')
boxyInstantiate = freshInstance Sigma

deconstructorInstantiate :: Sigma -> Tc Rho'
deconstructorInstantiate tfa@TForAll {} = do
    TForAll vs qt@(_ :=> t) <- freshSigma tfa
    let f (_ `TArrow` b) = f b
        f b = b
        eqvs = vs List.\\ freeVars (f t)
    tellTc mempty { existentialVars = eqvs }
    (_,t) <- freshInstance Sigma (TForAll (vs List.\\ eqvs) qt)
    return t
deconstructorInstantiate x = return x

boxySpec :: Sigma -> Tc ([(BoundTV,[Sigma'])],Rho')
boxySpec (TForAll as qt@(ps :=> t)) = do
    let f (TVar t) vs | t `elem` vs = do
            b <- lift (newBox $ tyvarKind t)
            tell [(t,b)]
            return b
        f e@TCon {} _ = return e
        f (TAp a b) vs = liftM2 tAp (f a vs) (f b vs)
        f (TArrow a b) vs = liftM2 TArrow (f a vs) (f b vs)
        f (TForAll as (ps :=> t)) vs = do
            t' <- f t (vs List.\\ as)
            return (TForAll as (ps :=> t'))
        f t _ = return t
        -- f t _ = error $ "boxySpec: " ++ show t
    (t',vs) <- runWriterT (f t as)
    addPreds $ inst mempty (Map.fromList [ (tyvarAtom bt,s) | (bt,s) <- vs ]) ps
    return (sortGroupUnderFG fst snd vs,t')




freeMetaVarsEnv :: Tc (Set.Set MetaVar)
freeMetaVarsEnv = do
    env <- asksTc tcCurrentEnv
    xs <- flip mapM (Map.elems env)  $ \ x -> do
        x <- flattenType x
        return $ freeMetaVars x
    return (Set.unions xs)

quantify :: [MetaVar] -> [Pred] -> Rho -> Tc Sigma
quantify vs ps r | not $ any isBoxyMetaVar vs = do
    vs <- mapM groundKind vs
    r <- flattenType r
    nvs <- mapM (newVar . fixKind . metaKind) vs
    sequence_ [ varBind mv (TVar v) | v <- nvs |  mv <- vs ]
    (ps :=> r) <- flattenType (ps :=> r)
    ch <- getClassHierarchy
    return $ TForAll nvs (FrontEnd.Tc.Class.simplify ch ps :=> r)

-- | Turn all @??@ into @*@ types, as we can't abstract over unboxed types
groundKind :: MetaVar -> Tc MetaVar
groundKind mv = zonkKind (fixKind $ metaKind mv) mv

fixKind :: Kind -> Kind
fixKind (KBase KQuestQuest) = KBase Star
fixKind (KBase KQuest) = KBase Star
fixKind (a `Kfun` b) = fixKind a `Kfun` fixKind b
fixKind x = x

-- | This removes all boxes, replacing them with tau vars
unBox ::  Type -> Tc Type
unBox tv = ft' tv where
    ft t@(TMetaVar mv)
        | isBoxyMetaVar mv = do
            tmv <- newMetaVar Tau (getType mv)
            varBind mv tmv
            return tmv
        | otherwise =  return t
    ft t = tickleM ft' t
    ft' t = evalType t >>= ft

evalType :: Type -> Tc Type
evalType t = findType t >>= evalTAssoc >>= evalArrowApp
evalFullType :: Type -> Tc Type
evalFullType t = f' t where
    f t = tickleM f' t
    f' t =  evalType t >>= f

evalTAssoc :: Type -> Tc Type
evalTAssoc ta@TAssoc { typeCon = Tycon { tyconName = n1 }, typeClassArgs = ~[carg], typeExtraArgs = eas }  = do
    carg' <- evalType carg
    case fromTAp carg' of
        (TCon Tycon { tyconName = n2 }, as) -> do
            InstanceEnv ie <- asksTc tcInstanceEnv
            case Map.lookup (n1,n2) ie of
                Just (aa,bb,tt) -> evalType (applyTyvarMap (zip aa as ++ zip bb eas) tt)
                _ -> fail "no instance for associated type"
        _ -> return ta { typeClassArgs = [carg'] }
evalTAssoc t = return t


evalArrowApp :: Type -> Tc Type
evalArrowApp (TAp (TAp (TCon tcon) ta) tb) 
    | tyconName tcon == tc_Arrow = return (TArrow ta tb) 

evalArrowApp t = return t


-- | Bind mv to type, first filling in any boxes in type with tau vars
varBind :: MetaVar -> Type -> Tc ()
varBind u t
--    | getType u /= getType t = error $ "varBind: kinds do not match:" ++ show (u,t)
    | otherwise = do
        kindCombine (getType u) (getType t)
        tt <- unBox t
        --(t,be,_) <- unbox t
        --when be $ error $ "binding boxy: " ++ tupled [pprint u,prettyPrintType t]
        tt <- evalFullType tt
        when (dump FD.BoxySteps) $ liftIO $ putStrLn $ "varBind: " ++ pprint u <+> text ":=" <+> prettyPrintType tt
        when (u `Set.member` freeMetaVars tt) $ do
            unificationError (TMetaVar u) tt -- occurs check
        let r = metaRef u
        x <- liftIO $ readIORef r
        case x of
            Just r -> fail $ "varBind: binding unfree: " ++ tupled [pprint u,prettyPrintType tt,prettyPrintType r]
            Nothing -> liftIO $ do
                --when (dump FD.BoxySteps) $ putStrLn $ "varBind: " ++ pprint u <+> text ":=" <+> prettyPrintType t
                writeIORef r (Just tt)


zonkKind :: Kind -> MetaVar -> Tc MetaVar
zonkKind nk mv = do
    fk <- kindCombine nk (metaKind mv)
    if fk == metaKind mv then return mv else do
        nref <- liftIO $ newIORef Nothing
        let nmv = mv { metaKind = fk, metaRef = nref }
        liftIO $ modifyIORef (metaRef mv) (\Nothing -> Just $ TMetaVar nmv)
        return nmv




zonkBox :: MetaVar -> Tc Type
zonkBox mv | isBoxyMetaVar mv = findType (TMetaVar mv)
zonkBox mv = fail $ "zonkBox: nonboxy" ++ show mv

readFilledBox :: MetaVar -> Tc Type
readFilledBox mv | isBoxyMetaVar mv = zonkBox mv >>= \v -> case v of
    TMetaVar mv' | mv == mv' -> fail $ "readFilledBox: " ++ show mv
    t -> return t
readFilledBox mv = error $ "readFilledBox: nonboxy" ++ show mv

elimBox :: MetaVar -> Tc Type
elimBox mv | isBoxyMetaVar mv = do
    t <- readMetaVar mv
    case t of
        Just t -> return t
        Nothing -> newMetaVar Tau (getType mv)

elimBox mv = error $ "elimBox: nonboxy" ++ show mv



----------------------------------------
-- Declaration of instances, boilerplate
----------------------------------------

--pretty  :: PPrint Doc a => a -> String
--pretty x  = show (pprint x :: Doc)

instance Monad Tc where
    return a = Tc $ return a
    Tc comp >>= fun = Tc $ do x <- comp; case fun x of Tc m -> m
    Tc a >> Tc b = Tc $ a >> b
    fail s = Tc $ do
        st <- ask
        liftIO $ processIOErrors
        Left x <- typeError (Failure s) (tcDiagnostics st)
        liftIO $ fail x

instance MonadWarn Tc where
    addWarning w = liftIO $ processErrors [w]

instance MonadSrcLoc Tc where
    getSrcLoc = do
        xs <- getErrorContext
        case xs of
            (Msg (Just sl) _:_) -> return sl
            _ -> return bogusASrcLoc

instance UniqueProducer Tc where
    newUniq = do
        v <- asksTc tcVarnum
        n <- liftIO $ do
            n <- readIORef v
            writeIORef v $! n + 1
            return n
        return n

tcInfoEmpty :: TcInfo
tcInfoEmpty = TcInfo {
    tcInfoEnv = mempty,
    tcInfoModName = "(unknown)",
    tcInfoKindInfo = mempty,
    tcInfoClassHierarchy = mempty,
    tcInfoSigEnv = mempty
}


withMetaVars :: MetaVar -> [Kind] -> ([Sigma] -> Sigma) -> ([Sigma'] -> Tc a) -> Tc a
withMetaVars mv ks sfunc bsfunc | isBoxyMetaVar mv = do
    boxes <- mapM newBox ks
    res <- bsfunc boxes
    tys <- mapM readFilledBox [ mv | ~(TMetaVar mv) <- boxes]
    varBind mv (sfunc tys)
    return res
withMetaVars mv ks sfunc bsfunc  = do
    taus <- mapM (newMetaVar Tau) ks
    varBind mv (sfunc taus)
    bsfunc taus