module FrontEnd.Class(
    printClassHierarchy,
    instanceToTopDecls,
    ClassHierarchy,
    ClassRecord(..),
    instanceName,
    defaultInstanceName,
    printClassSummary,
    findClassRecord,
    asksClassRecord,
    classRecords,
    makeClassHierarchy,
    derivableClasses,
    makeInstanceEnv,
    InstanceEnv(..),
    Inst(..)
    ) where

import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid
import Data.Generics
import Data.List((\\),partition,nub)
import Text.PrettyPrint.HughesPJ(render,Doc())
import qualified Data.Map as Map
import qualified Text.PrettyPrint.HughesPJ as PPrint

import Binary
import Doc.DocLike
import Doc.PPrint
import FrontEnd.KindInfer
import FrontEnd.SrcLoc
import FrontEnd.Tc.Type
import FrontEnd.Utils
import HsSyn
import MapBinaryInstance()
import Maybe
import Monad
import Name.Name
import Name.Names
import Name.VConsts
import Options
import Support.CanType
import PrimitiveOperators(primitiveInsts)
import Support.FreeVars
import Util.Gen
import Util.HasSize
import Util.Inst()
import Util.SetLike
import qualified FlagOpts as FO
import Support.Tickle

--------------------------------------------------------------------------------

type Assump = (Name,Sigma)

data Inst = Inst {
    instSrcLoc :: SrcLoc,
    instDerived :: Bool,   -- ^ whether this instance was derived
    instHead :: Qual Pred,
    instAssocs :: [(Tycon,[Tyvar],[Tyvar],Sigma)]
    } deriving(Eq,Ord,Show)
    {-! derive: GhcBinary !-}

instance PPrint a (Qual Pred) => PPrint a Inst where
    pprint Inst { instHead = h, instAssocs = [] } = pprint h
    pprint Inst { instHead = h, instAssocs = as } = pprint h <+> text "where" <$> vcat [ text "    type" <+> pprint n <+> text "_" <+> hsep (map pprint ts) <+> text "=" <+> pprint sigma  | (n,_,ts,sigma) <- as]


emptyInstance = Inst { instDerived = False, instSrcLoc = bogusASrcLoc, instHead = error "emptyInstance", instAssocs = [] }

-- | a class record is either a class along with instances, or just instances.
-- you can tell the difference by the presence of the classArgs field

data ClassRecord = ClassRecord {
    className :: Class,
    classSrcLoc :: SrcLoc,
    classArgs :: [Tyvar],
    classSupers :: [Class],
    classInsts :: [Inst],
    classAssumps :: [(Name,Sigma)],
    classAssocs :: [(Tycon,[Tyvar],Maybe Sigma)]
    }
    {-! derive: GhcBinary !-}

newClassRecord c = ClassRecord {
    className = c,
    classSrcLoc = bogusASrcLoc,
    classSupers = [],
    classArgs = [],
    classInsts = [],
    classAssumps = [],
    classAssocs = []
    }

combineClassRecords cra crb | className cra == className crb = ClassRecord {
    className = className cra,
    classSrcLoc = if classSrcLoc cra == bogusASrcLoc then classSrcLoc crb else classSrcLoc cra,
    classSupers = snub $ classSupers cra ++ classSupers crb,
    classInsts = snub $ classInsts cra ++ classInsts crb,
    classAssumps = snubFst $ classAssumps cra ++ classAssumps crb,
    classAssocs = snubUnder fst3 $ classAssocs cra ++ classAssocs crb,
    classArgs = if null (classArgs cra) then classArgs crb else classArgs cra
    }

fst3 (x,_,_) = x


newtype InstanceEnv = InstanceEnv { instanceEnv :: Map.Map (Name,Name) ([Tyvar],[Tyvar],Type) }

makeInstanceEnv :: ClassHierarchy -> InstanceEnv
makeInstanceEnv (ClassHierarchy ch) = InstanceEnv $ Map.fromList (concatMap f (Map.elems ch)) where
    f cr = concatMap (g cr) (classInsts cr)
    g cr Inst { instHead = _ :=> IsIn _cname tt, instAssocs = as } | _cname == className cr = ans where
        ans = [ ((tyconName tc,getTypeHead tt),(is,rs,e)) | (tc,is,rs,e) <- as]
    g cr x = error $  "makeInstanceEnv: " ++ show (className cr,x)

getTypeHead th = case fromTAp th of
    (TArrow {},_) -> tc_Arrow
    (TCon c,_) -> tyconName c
    _ -> error $ "getTypeHead: " ++ show th

newtype ClassHierarchy = ClassHierarchy (Map.Map Class ClassRecord)
    deriving (Binary,HasSize)

instance Monoid ClassHierarchy where
    mempty = ClassHierarchy mempty
    mappend (ClassHierarchy a) (ClassHierarchy b) = ClassHierarchy $ Map.unionWith combineClassRecords a b

classRecords :: ClassHierarchy -> [ClassRecord]
classRecords (ClassHierarchy ch) = Map.elems ch

findClassRecord (ClassHierarchy ch) cn = case Map.lookup cn ch of
    Nothing -> error $ "findClassRecord: " ++ show cn
    Just n -> n

asksClassRecord (ClassHierarchy ch) cn f = case Map.lookup cn ch of
    Nothing -> error $ "asksClassRecord: " ++ show cn
    Just n -> f n

showInst :: Inst -> String
showInst = PPrint.render . pprint


aHsTypeSigToAssumps :: KindEnv -> HsDecl -> [(Name,Type)]
aHsTypeSigToAssumps kt sig@(HsTypeSig _ names qualType) = [ (toName Val n,typ) | n <- names] where
    Identity typ = hsQualTypeToSigma kt qualType


qualifyMethod :: [HsAsst] -> HsDecl -> HsDecl
qualifyMethod [HsAsst c [n]] (HsTypeSig sloc names (HsQualType oc t))
    = HsTypeSig sloc names (HsQualType (HsAsst c [n']:oc) t) where
        Just n' = (something (mkQ mzero f)) t
        f (HsTyVar n') | hsNameToOrig n' == hsNameToOrig n = return n'
        f _ = mzero



printClassSummary :: ClassHierarchy -> IO ()
printClassSummary (ClassHierarchy h) = mapM_ f $  h' where
    h' = [ (n,runIdentity $ Map.lookup n h) | n <- (map fst [ (cn, classSupers ss) | (cn,ss) <- Map.toList h]) ]
    f (cname, (ClassRecord { classSupers = supers, classInsts = insts, classAssumps = ma})) = do
        putStrLn $ "-- class: " ++ show cname
        unless (null supers) $ putStrLn $ "super classes:" ++ unwords (map show supers)
        unless (null insts) $ putStrLn $ "instances: " ++ (concatInter ", " (map showInst insts))
        putStrLn ""



printClassHierarchy :: ClassHierarchy -> IO ()
printClassHierarchy (ClassHierarchy h) = mapM_ printClassDetails $  Map.toList h where
    printClassDetails :: (Name, ClassRecord) -> IO ()
    printClassDetails (cname, (ClassRecord { classArgs = classArgs, classSupers = supers, classInsts = insts, classAssumps = methodAssumps, classAssocs = classAssocs})) = do
        putStrLn "..........."
        putStrLn $ "class: " ++ hsep (pprint cname:map pprint classArgs)
        putStr $ "super classes:"
        pnone supers $ do putStrLn $ " " ++ (concatInter " " (map show supers))
        putStr $ "instances:"
        pnone insts $  putStr $ "\n" ++ (showListAndSepInWidth showInst 80 ", " insts)
        putStr $ "method signatures:"
        pnone methodAssumps $ putStr $ "\n" ++ (unlines $ map pretty methodAssumps)
        putStr $ "associated types:"
        pnone classAssocs $  putStrLn $ "\n" ++ (unlines $ map (render . passoc) classAssocs)
        putStr "\n"
    pnone [] f = putStrLn " none"
    pnone xs f = f
    passoc (nk,as,mt) = text "type" <+> pprint nk <+> hsep (map pprint as) <> case mt of
        Nothing -> empty
        Just s -> text " = " <> pprint s


--------------------------------------------------------------------------------



modifyClassRecord ::  (ClassRecord -> ClassRecord) -> Class -> ClassHierarchy -> ClassHierarchy
modifyClassRecord f c (ClassHierarchy h) = case Map.lookup c h of
           --Nothing -> error $ "modifyClassRecord: " ++ show c
           Nothing -> ClassHierarchy $ Map.insert c (f (newClassRecord c)) h
           Just r -> ClassHierarchy $ Map.insert c (f r) h

addOneInstanceToHierarchy :: ClassHierarchy -> Inst -> ClassHierarchy
addOneInstanceToHierarchy ch inst@Inst { instHead = cntxt :=> IsIn className _ } = modifyClassRecord f className ch where
    f c = c { classInsts = inst:classInsts c }


hsInstDeclToInst :: Monad m => KindEnv -> HsDecl -> m [Inst]
hsInstDeclToInst kt (HsInstDecl sloc qType decls)
   | classKind == argTypeKind
        = return [emptyInstance { instSrcLoc = sloc, instDerived = False, instHead = cntxt :=> IsIn className convertedArgType, instAssocs = assocs }]
   | otherwise = failSl sloc $ "hsInstDeclToInst: kind error, attempt to make\n" ++
                      show convertedArgType ++ " (with kind " ++ show argTypeKind ++ ")\n" ++
                      "an instance of class " ++ show className ++
                      " (with kind " ++ show classKind ++ ")"
   where
   (cntxt, (className, cargs@[convertedArgType])) = qtToClassHead kt qType
   classKind = kindOfClass className kt
   argTypeKind = map getType cargs
   assocs = [ (tc,as,bs,s) | (tc,as,bs,~(Just s)) <- createInstAssocs kt decls ]
hsInstDeclToInst _ _ = return []


{-
-- derive statements
hsInstDeclToInst kt (HsDataDecl _sloc _cntxt tyConName argNames _condecls derives@(_:_))
   = return $ map ((,) True) newInstances
   where
   tyConKind = kindOf (toName TypeConstructor tyConName) kt
   flatTyConKind = unfoldKind tyConKind
   argTypeKind = foldr1 Kfun $ drop (length argNames) flatTyConKind
   argsAsTypeList = map (\n -> HsTyVar n) argNames
   typeKindPairs :: [(HsType, Kind)]
   typeKindPairs = (HsTyCon tyConName, tyConKind) : zip argsAsTypeList flatTyConKind
   convertedType :: Type
   convertedType = convType typeKindPairs
   --newContext = map (hsAsstToPred kt) cntxt
   --newInstances = makeDeriveInstances newContext convertedType derives
   newInstances = mempty

hsInstDeclToInst kt (HsNewTypeDecl _sloc _cntxt tyConName argNames _condecls derives@(_:_))
   = return $ map ((,) True) newInstances
   where
   tyConKind = kindOf (toName TypeConstructor tyConName) kt
   flatTyConKind = unfoldKind tyConKind
   argTypeKind = foldr1 Kfun $ drop (length argNames) flatTyConKind
   argsAsTypeList = map (\n -> HsTyVar n) argNames
   typeKindPairs :: [(HsType, Kind)]
   typeKindPairs = (HsTyCon tyConName, tyConKind) : zip argsAsTypeList flatTyConKind
   convertedType :: Type
   convertedType = convType typeKindPairs
   --newContext = map (hsAsstToPred kt) cntxt
   --newInstances = makeDeriveInstances newContext convertedType derives
   newInstances = mempty


-- the types will only ever be constructors or vars

convType :: [(HsType, Kind)] -> Type
convType tsks = foldl1 TAp (map toType tsks)

toType :: (HsType, Kind) -> Type
toType (HsTyCon n, k) = TCon $ Tycon (toName TypeConstructor n) k
toType (HsTyVar n, k) = TVar $ tyvar (toName TypeVal n) k
toType (HsTyFun x y, Star) = TArrow (toType (x,Star)) (toType (y,Star))
toType x = error $ "toType: " ++ show x
-}





qtToClassHead :: KindEnv -> HsQualType -> ([Pred],(Name,[Type]))
qtToClassHead kt (HsQualType cntx (HsTyApp (HsTyCon className) ty)) = (map (hsAsstToPred kt) cntx,(toName ClassName className,[runIdentity $ hsTypeToType kt ty]))

createClassAssocs kt decls = [ (ctc n,map ct as,ctype t)| HsTypeDecl { hsDeclName = n, hsDeclTArgs = as, hsDeclType = t } <- decls ] where
    ctc n = let nn = toName TypeConstructor n in Tycon nn (kindOf nn kt)
    ct (HsTyVar n) = let nn = toName TypeVal n in tyvar nn (kindOf nn kt)
    ctype HsTyAssoc = Nothing
    ctype t = Just $ runIdentity $ hsTypeToType kt t

createInstAssocs kt decls = [ (ctc n,map ct (czas ca),map ct as,ctype t)| HsTypeDecl { hsDeclName = n, hsDeclTArgs = (ca:as), hsDeclType = t } <- decls ] where
    ctc n = let nn = toName TypeConstructor n in Tycon nn (kindOf nn kt)
    ct (HsTyVar n) = let nn = toName TypeVal n in tyvar nn (kindOf nn kt)
    czas ca = let (HsTyCon {},zas) = fromHsTypeApp ca in zas
    ctype HsTyAssoc = Nothing
    ctype t = Just $ runIdentity $ hsTypeToType kt t

fromHsTypeApp t = f t [] where
    f (HsTyApp a b) rs = f a (b:rs)
    f t rs = (t,rs)

instanceToTopDecls :: KindEnv -> ClassHierarchy -> HsDecl -> (([HsDecl],[Assump]))
instanceToTopDecls kt (ClassHierarchy classHierarchy) (HsInstDecl _ qualType methods)
    = unzip $ map (methodToTopDecls kt [] crecord qualType) $ methodGroups where
    methodGroups = groupEquations methods
    (_,(className,_)) = qtToClassHead kt qualType
    crecord = case Map.lookup className classHierarchy  of
        Nothing -> error $ "instanceToTopDecls: could not find class " ++ show className ++ "in class hierarchy"
        Just crecord -> crecord
    tsubst na vv v = applyTyvarMap [(na,vv)] v

instanceToTopDecls kt (ClassHierarchy classHierarchy) (HsClassDecl _ qualType methods)
   = unzip $ map (defaultMethodToTopDecls kt methodSigs qualType) $ methodGroups where
   HsQualType _ (HsTyApp (HsTyCon className) _) = qualType
   methodGroups = groupEquations (filter (\x -> isHsPatBind x || isHsFunBind x)  methods)
   methodSigs = case Map.lookup (toName ClassName className) classHierarchy  of
           Nothing -> error $ "defaultInstanceToTopDecls: could not find class " ++ show className ++ "in class hierarchy"
           Just sigs -> classAssumps sigs
instanceToTopDecls _ _ _ = mempty



getHsTypeCons (HsTyCon n) = n
getHsTypeCons (HsTyApp a _) = getHsTypeCons a
getHsTypeCons (HsTyFun {}) = nameName (tc_Arrow)
getHsTypeCons (HsTyTuple xs) = toTuple (length xs)
getHsTypeCons x = error $ "getHsTypeCons: " ++ show x


instanceName n t = toName Val $ Qual (Module "Instance@") $ HsIdent ('i':show n ++ "." ++ show t)
defaultInstanceName n = toName Val $ Qual (Module "Instance@") $ HsIdent ('i':show n ++ ".default")


methodToTopDecls ::
    KindEnv            -- ^ the kindenv
    -> [Pred]          -- ^ random extra predicates to add
    -> ClassRecord     -- ^ the class we are lifting methods from
    -> HsQualType
    -> (Name, HsDecl)
    -> (HsDecl,Assump)

methodToTopDecls kt preds crecord qt (methodName, methodDecls)
   = (renamedMethodDecls,(newMethodName, instantiatedSig)) where
    (cntxt,(className,[argType])) = qtToClassHead kt qt
    newMethodName = instanceName methodName (getTypeHead argType)
    sigFromClass = case [ s | (n, s) <- classAssumps crecord, n == methodName] of
        [x] -> x
        _ -> error $ "sigFromClass: " ++ pprint (classAssumps crecord) ++ " " ++ show  methodName
    instantiatedSig = newMethodSig' kt methodName (preds ++ cntxt) sigFromClass argType
    renamedMethodDecls = renameOneDecl newMethodName methodDecls

defaultMethodToTopDecls :: KindEnv -> [Assump] -> HsQualType -> (Name, HsDecl) -> (HsDecl,Assump)

defaultMethodToTopDecls kt methodSigs (HsQualType cntxt classApp) (methodName, methodDecls)
   = (renamedMethodDecls,(newMethodName,sigFromClass)) where
    (HsTyApp (HsTyCon className) _) = classApp
    newMethodName = defaultInstanceName methodName
    sigFromClass = case [ s | (n, s) <- methodSigs, n == methodName] of
        [x] -> x
        _ -> error $ "sigFromClass: " ++ show methodSigs ++ " " ++ show  methodName
     --  = newMethodSig cntxt newMethodName sigFromClass argType
    renamedMethodDecls = renameOneDecl newMethodName methodDecls

renameOneDecl :: Name -> HsDecl -> HsDecl
renameOneDecl newName (HsFunBind matches)
   = HsFunBind  (map (renameOneMatch newName) matches)
-- all pattern bindings are simple by this stage
-- (ie no compound patterns)
renameOneDecl newName (HsPatBind sloc (HsPVar patName) rhs wheres)
   = HsPatBind sloc (HsPVar (nameName newName)) rhs wheres

renameOneMatch :: Name -> HsMatch -> HsMatch
renameOneMatch newName (HsMatch sloc oldName pats rhs wheres)
   = HsMatch sloc (nameName newName) pats rhs wheres



newMethodSig' :: KindEnv -> Name -> [Pred] -> Sigma -> Type -> Sigma
newMethodSig' kt methodName newCntxt qt' instanceType  = newQualType where
    TForAll _ ((IsIn _ classArg:restContext) :=> t) = qt'
    -- the assumption is that the context is non-empty and that
    -- the class and variable that we are interested in are at the
    -- front of the old context - the method of inserting instance types into
    -- the class hierarchy should ensure this
    --((className, classArg):restContxt) = cntxt
    foo = "_" ++ (show methodName ++ show (getTypeHead instanceType)) ++ "@@"
--    newQualType = everywhere (mkT at) $ tForAll (nub $ freeVars qt) qt

    newQualType = tForAll vs nqt where
        vs = nub $ freeVars nqt
        nqt = map (tickle f) (newCntxt ++ restContext) :=> f t
        f t | t == classArg = f instanceType
        f (TVar t) = TVar (at t)
        f (TForAll ta (ps :=> t)) = tickle f (TForAll (map at ta) (ps :=> t))
        f (TExists ta (ps :=> t)) = tickle f (TExists (map at ta) (ps :=> t))
        f t = tickle f t

    at (Tyvar _ n k) =  tyvar (updateName (++ foo) n) k
    updateName f n = toName nt (md,f nm) where
         (nt,(md::String,nm)) = fromName n
--    qt = (newCntxt ++ restContext) :=> t
    {-
    qt = (newCntxt ++ restContext) :=> (everywhere (mkT ct) t)
    ct n | n == classArg = instanceType
    ct n =  n
    -}


-- collect assumptions of all class methods

classMethodAssumps :: ClassHierarchy -> [Assump]
classMethodAssumps hierarchy = concatMap classAssumps $ classRecords hierarchy

--------------------------------------------------------------------------------


failSl sl m = fail $ show sl ++ ": " ++ m

classHierarchyFromRecords rs =  ClassHierarchy $ Map.fromListWith combineClassRecords [  (className x,x)| x <- rs ]

-- I love tying el knot.
makeClassHierarchy :: Monad m => ClassHierarchy -> KindEnv -> [HsDecl] -> m ClassHierarchy
makeClassHierarchy (ClassHierarchy ch) kt ds = return (ClassHierarchy ans) where
    ans =  Map.fromListWith combineClassRecords [  (className x,x)| x <- execWriter (mapM_ f ds) ]
    f (HsClassDecl sl t decls)
        | HsTyApp (HsTyCon className) (HsTyVar argName)  <- tbody = do
            let qualifiedMethodAssumps = concatMap (aHsTypeSigToAssumps kt . qualifyMethod newClassContext) (filter isHsTypeSig decls)
                newClassContext = [HsAsst className [argName]] -- hsContextToContext [(className, argName)]
            tell [ClassRecord { classArgs = classArgs, classAssocs = classAssocs, className = toName ClassName className, classSrcLoc = sl, classSupers = [ toName ClassName x | HsAsst x _ <- cntxt], classInsts = [ emptyInstance { instHead = i } | i@(_ :=> IsIn n _) <- primitiveInsts, nameName n == className], classAssumps = qualifiedMethodAssumps }]
        | otherwise = failSl sl "Invalid Class declaration."
        where
        HsQualType cntxt tbody = t
        classAssocs = createClassAssocs kt decls
        (_,(_,classArgs')) = qtToClassHead kt t
        classArgs = [ v | ~(TVar v) <- classArgs' ]
    f decl = hsInstDeclToInst kt decl >>= \insts -> do
        crs <- flip mapM [ (cn,i) | i@Inst { instHead = _ :=> IsIn cn _} <- insts] $ \ (x,inst) -> case Map.lookup x ch of
            Just cr -> ensureNotDup (srcLoc decl) inst (classInsts cr) >> return [cr { classInsts = mempty }]
            Nothing -> return [] -- case Map.lookup x ans of
                -- Just _ -> return []
               --  Nothing -> return [] -- failSl (srcLoc decl) "Invalid Instance"
        case foldl addOneInstanceToHierarchy (classHierarchyFromRecords (concat crs)) insts of
                ClassHierarchy ch -> tell $ Map.elems ch
    f _ = return ()


ensureNotDup :: Monad m => SrcLoc -> Inst -> [Inst] -> m ()
ensureNotDup sl i is | i `elem` is = failSl sl $ "Duplicate Instance: " ++ show i
                     | otherwise = return ()


accLen :: Int -> [[a]] -> [(Int, [a])]
accLen width [] = []
accLen width (x:xs) = let newWidth = length x + width in (newWidth, x) : accLen newWidth xs

groupStringsToWidth :: Int -> [String] -> [String]
groupStringsToWidth width ss
   = groupStringsToWidth' width (accLen 0 ss)
   where
   groupStringsToWidth' :: Int -> [(Int,String)] -> [String]
   groupStringsToWidth' width [] = []
   groupStringsToWidth' width xs
      = headString : groupStringsToWidth' width (accLen 0 $ map snd rest)
      where
      (headSegments, rest)
         = case span ((<=width).fst) xs of
              ([], ss)     -> ([head ss], tail ss)
              anythingElse -> anythingElse
      headString = concatMap snd headSegments

showListAndSepInWidth :: (a -> String) -> Int -> String -> [a] -> String
showListAndSepInWidth _ _ _ [] = []
showListAndSepInWidth f width sep things = unlines $ groupStringsToWidth width newThings where
   newThings = (map ((\t -> t ++ sep).f) (init things)) ++ [f (last things)]

pretty  :: PPrint Doc a => a -> String
pretty   = render . pprint

nameOfTyCon :: NameType -> HsType -> Name
nameOfTyCon t (HsTyCon n) = toName t n
nameOfTyCon t (HsTyTuple xs) = nameTuple t (length xs)
nameOfTyCon t (HsTyFun _ _) = tc_Arrow
nameOfTyCon _ t = error $ "nameOfTyCon: " ++ show t

groupEquations :: [HsDecl] -> [(Name, HsDecl)]
groupEquations [] = []
groupEquations (HsTypeDecl {}:ds) = groupEquations ds
groupEquations (d:ds) = (getDeclName d, d) : groupEquations ds



derivableClasses ::  [Name]

derivableClasses = [
    class_Eq,
    class_Ord,
    class_Enum,
    class_Bounded,
    class_Show,
    class_Read
    ]
