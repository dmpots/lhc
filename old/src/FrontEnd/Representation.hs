{-
        Copyright:        Mark Jones and The Hatchet Team
                          (see file Contributors)
        Module:           Representation
        Primary Authors:  Mark Jones and Bernie Pope
        Description:      The basic data types for representing objects
                          in the type inference algorithm.
        Notes:            See the file License for license information
                          Large parts of this module were derived from
                          the work of Mark Jones' "Typing Haskell in
                          Haskell", (http://www.cse.ogi.edu/~mpj/thih/)
-}

module FrontEnd.Representation(
    Type(..),
    Tyvar(..),
    tyvar,
    Tycon(..),
    fn,
    Pred(..),
    Qual(..),
    Class,
    tForAll,
    tExists,
    MetaVarType(..),
    prettyPrintType,
    fromTAp,
    fromTArrow,
    tassocToAp,
    splitTAp_maybe,
    MetaVar(..),
    tTTuple,
    tTTuple',
    tList,
    tArrow,
    tAp
    )where

import Data.DeriveTH
import Data.Derive.All
import Control.Monad.Identity
import Data.IORef

import StringTable.Atom
import Data.Binary
import Doc.DocLike
import Doc.PPrint(pprint,PPrint)
import FrontEnd.HsSyn
import Name.Name
import Name.Names
import Support.CanType
import Name.VConsts
import qualified Doc.DocLike as D
import Support.Unparse
import Util.VarName
import FrontEnd.Tc.Kind


--------------------------------------------------------------------------------

-- Types

data MetaVarType = Tau | Rho | Sigma
             deriving(Eq,Ord,Show)

data Type  = TVar { typeVar :: {-# UNPACK #-} !Tyvar }
           | TCon { typeCon :: !Tycon }
           -- | Please don't call @TAp@; use 'tAp' instead.
           | TAp  Type Type
           | TArrow Type Type
           | TForAll { typeArgs :: [Tyvar], typeBody :: (Qual Type) }
           | TExists { typeArgs :: [Tyvar], typeBody :: (Qual Type) }
           | TMetaVar { metaVar :: MetaVar } -- ^ Used only in the typechecker
           | TAssoc   { typeCon :: !Tycon, typeClassArgs :: [Type], typeExtraArgs :: [Type] }
             deriving(Ord,Show)


-- | Used only in the typechecker
data MetaVar = MetaVar { metaUniq :: !Int, metaKind :: Kind, metaRef :: (IORef (Maybe Type)), metaType :: MetaVarType }
             deriving(Show)

instance Eq MetaVar where
    a == b = metaUniq a == metaUniq b

instance Ord MetaVar where
    compare a b = compare (metaUniq a) (metaUniq b)

instance TypeNames Type where
    tBool = TCon (Tycon tc_Bool kindStar)
    tString = TAp tList tChar
    tChar      = TCon (Tycon tc_Char kindStar)
    tUnit = TCon (Tycon tc_Unit kindStar)

-- | Dummy instance. We'll never actually serialize a 'MetaVar'.
--
-- FIXME: Prove this statically.
instance Binary MetaVar where
  get = error "get not defined for MetaVar"
  put = error "put not defined for MetaVar"

-- | The @[]@ type constructor
tList :: Type
tList = TCon (Tycon tc_List (Kfun kindStar kindStar))

-- | The @(->)@ type constructor. Invariant: @tArrow@ shall not be fully applied. To this end, see 'tAp'.
tArrow :: Type
tArrow = TCon (Tycon tc_Arrow (kindArg `Kfun` kindFunRet `Kfun` kindStar))

instance Eq Type where
    (TVar a) == (TVar b) = a == b
    (TMetaVar a) == (TMetaVar b) = a == b
    (TCon a) == (TCon b) = a == b
    (TAp a' a) == (TAp b' b) = a' == b' && b == a
    (TArrow a' a) == (TArrow b' b) = a' == b' && b == a
    _ == _ = False

-- | Type application, enforcing the invariant that there be no fully-applied 'tArrow's
tAp :: Type -> Type -> Type
tAp (TAp c@TCon{} a) b | c == tArrow = TArrow a b
tAp a b = TAp a b

tassocToAp :: Type -> Type
tassocToAp TAssoc { typeCon = con, typeClassArgs = cas, typeExtraArgs = eas } = foldl tAp (TCon con) (cas ++ eas)


-- | Unquantified type variable
data Tyvar = Tyvar { tyvarAtom :: {-# UNPACK #-} !Atom, tyvarName ::  !Name, tyvarKind :: Kind }

instance Show Tyvar where
    showsPrec _ Tyvar { tyvarName = hn, tyvarKind = k } = shows hn . ("::" ++) . shows k

tForAll :: [Tyvar] -> Qual Type -> Type
tForAll [] ([] :=> t) = t
tForAll vs (ps :=> TForAll vs' (ps' :=> t)) = tForAll (vs ++ vs') ((ps ++ ps') :=> t)
tForAll x y = TForAll x y

tExists :: [Tyvar] -> Qual Type -> Type
tExists [] ([] :=> t) = t
tExists vs (ps :=> TExists vs' (ps' :=> t)) = tExists (vs ++ vs') ((ps ++ ps') :=> t)
tExists x y = TExists x y


instance Show (IORef a) where
    showsPrec _ _ = ("<IORef>" ++)

tyvar :: Name -> Kind -> Tyvar
tyvar n k = Tyvar (toAtom $ show n) n k

instance Eq Tyvar where
    Tyvar { tyvarAtom = x } == Tyvar { tyvarAtom = y } = x == y
    Tyvar { tyvarAtom = x } /= Tyvar { tyvarAtom = y } = x /= y

instance Ord Tyvar where
    compare (Tyvar { tyvarAtom = x }) (Tyvar { tyvarAtom = y }) = compare x y
    (Tyvar { tyvarAtom = x }) <= (Tyvar { tyvarAtom = y }) = x <= y
    (Tyvar { tyvarAtom = x }) >= (Tyvar { tyvarAtom = y }) = x >= y
    (Tyvar { tyvarAtom = x }) <  (Tyvar { tyvarAtom = y })  = x < y
    (Tyvar { tyvarAtom = x }) >  (Tyvar { tyvarAtom = y })  = x > y



-- | Type constructor
data Tycon = Tycon { tyconName :: Name, tyconKind :: Kind }
    deriving(Eq, Show,Ord)

instance ToTuple Tycon where
    toTuple n = Tycon (nameTuple TypeConstructor n) (foldr Kfun kindStar $ replicate n kindStar)
instance ToTuple Type where
    toTuple n = TCon $ toTuple n

instance DocLike d => PPrint d Tycon where
   pprint (Tycon i _) = pprint i


-- | Build a function type
infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TArrow a b

--------------------------------------------------------------------------------


-- | Predicate
data Pred   = IsIn Class Type | IsEq Type Type
              deriving(Show, Eq,Ord)

-- | Qualified entities
data Qual t =  [Pred] :=> t
              deriving(Show, Eq,Ord)


instance (DocLike d,PPrint d t) => PPrint d (Qual t) where
    pprint ([] :=> r) = pprint r
    pprint ([x] :=> r) = pprint x <+> text "=>" <+> pprint r
    pprint (xs :=> r) = tupled (map pprint xs) <+> text "=>" <+> pprint r




type Class = Name
--------------------------------------------------------------------------------



instance  DocLike d => PPrint d Tyvar where
  pprint tv = tshow (tyvarName tv)

instance Binary Tyvar where
    put (Tyvar aa ab ac) = do
        put aa
        put ab
        put ac
    get = do
        aa <- get
        ab <- get
        ac <- get
        return (Tyvar aa ab ac)


instance FromTupname HsName where
    fromTupname (Qual (Module "Lhc.Basics") (HsIdent xs))  = fromTupname xs
    fromTupname _ = fail "fromTupname: not Prelude"

instance ToTuple HsName where
    toTuple n = (Qual (Module "Lhc.Basics") (HsIdent $ toTuple n))

-- pretty printing a HsName, Module and HsIdentifier

instance DocLike d => PPrint d HsName where
   pprint (Qual mod ident)
      -- don't print the Prelude module qualifier
      | mod == Module "Prelude" = pprint ident
      | otherwise               = pprint mod <> text "." <> pprint ident
   pprint (UnQual ident)
      = pprint ident

instance DocLike d => PPrint d Module where
   pprint (Module s) = text s

instance DocLike d => PPrint d HsIdentifier where
   pprint (HsIdent   s) = text s

instance DocLike d => PPrint d Type where
    pprint = prettyPrintType

withNewNames :: Monad m =>
                [Tyvar]
                -> ([String] -> VarNameT Kind Tyvar String m a)
                -> VarNameT Kind Tyvar String m a
withNewNames ts action = subVarName $ do
    ts' <- mapM newTyvarName ts
    action ts'

newTyvarName :: Monad m => Tyvar -> VarNameT Kind Tyvar String m String
newTyvarName t = case tyvarKind t of
    x@(KBase Star) -> newLookupName (map (:[]) ['a' ..]) x t
    y@(KBase Star `Kfun` KBase Star) -> newLookupName (map (('f':) . show) [0 :: Int ..]) y t
    z@(KBase KUTuple) -> newLookupName (map (('u':) . show) [0 :: Int ..]) z t
    z@(KBase KQuest) -> newLookupName (map (('q':) . show) [0 :: Int ..]) z t
    z@(KBase KQuestQuest) -> newLookupName (map (('q':) . ('q':) . show) [0 :: Int ..]) z t
    z -> newLookupName (map (('t':) . show) [0 :: Int ..]) z t


prettyPrintType :: DocLike d => Type -> d
prettyPrintType t  = unparse $ runIdentity (runVarNameT (f t)) where
    arr = bop (R,0) (space <> text "->" <> space)
    app = bop (L,100) (text " ")
    fp (IsIn cn t) = do
        t' <- f t
        return (atom (text $ show cn) `app` t')
    fp (IsEq t1 t2) = do
        t1' <- f t1
        t2' <- f t2
        return (atom (parens $ unparse t1' <+> text "=" <+> unparse t2'))
    f (TForAll [] ([] :=> t)) = f t
    f (TForAll vs (ps :=> t)) = do
        withNewNames vs $ \ts' -> do
        t' <- f t
        ps' <- mapM fp ps
        return $ case ps' of
            [] ->  fixitize (N,-3) $ pop (text "forall" <+> hsep (map text ts') <+> text ". ")  (atomize t')
            [p] -> fixitize (N,-3) $ pop (text "forall" <+> hsep (map text ts') <+> text "." <+> unparse p <+> text "=> ")  (atomize t')
            ps ->  fixitize (N,-3) $ pop (text "forall" <+> hsep (map text ts') <+> text "." <+> tupled (map unparse ps) <+> text "=> ")  (atomize t')
    f (TExists [] ([] :=> t)) = f t
    f (TExists vs (ps :=> t)) = do
        withNewNames vs $ \ts' -> do
        t' <- f t
        ps' <- mapM fp ps
        return $ case ps' of
            [] ->  fixitize (N,-3) $ pop (text "exists" <+> hsep (map text ts') <+> text ". ")  (atomize t')
            [p] -> fixitize (N,-3) $ pop (text "exists" <+> hsep (map text ts') <+> text "." <+> unparse p <+> text "=> ")  (atomize t')
            ps ->  fixitize (N,-3) $ pop (text "exists" <+> hsep (map text ts') <+> text "." <+> tupled (map unparse ps) <+> text "=> ")  (atomize t')
    f (TCon tycon) = return $ atom (pprint tycon)
    f (TVar tyvar) = do
        vo <- maybeLookupName tyvar
        case vo of
            Just c  -> return $ atom $ text c
            Nothing -> return $ atom $ tshow (tyvarAtom tyvar)
    f (TAp (TCon (Tycon n _)) x) | n == tc_List = do
        x <- f x
        return $ atom (char '[' <> unparse x <> char ']')
    f TAssoc { typeCon = con, typeClassArgs = cas, typeExtraArgs = eas } = do
        let x = atom (pprint con)
        xs <- mapM f (cas ++ eas)
        return $ foldl app x xs
    f ta@(TAp {}) | (TCon (Tycon c _),xs) <- fromTAp ta, Just _ <- fromTupname c = do
        xs <- mapM f xs
        return $ atom (tupled (map unparse xs))
    f (TAp t1 t2) = do
        t1 <- f t1
        t2 <- f t2
        return $ t1 `app` t2
    f (TArrow t1 t2) = do
        t1 <- f t1
        t2 <- f t2
        return $ t1 `arr` t2
    f (TMetaVar mv) = return $ atom $ pprint mv
--    f tv = return $ atom $ parens $ text ("FrontEnd.Tc.Type.pp: " ++ show tv)


instance DocLike d => PPrint d MetaVarType where
    pprint  t = case t of
        Tau -> char 't'
        Rho -> char 'r'
        Sigma -> char 's'



instance DocLike d => PPrint d Pred where
    pprint (IsIn c t) = text (show c) <+> prettyPrintType t
    pprint (IsEq t1 t2) = parens $ prettyPrintType t1 <+> text "=" <+> prettyPrintType t2

instance DocLike d => PPrint d MetaVar where
    pprint MetaVar { metaUniq = u, metaKind = k, metaType = t }
        | KBase Star <- k =  pprint t <> tshow u
        | otherwise = parens $ pprint t <> tshow u <> text " :: " <> pprint k

-- | Return the head and arguments of whatever arity application was
-- given. (Non-applications are just 0-arity applications.)
fromTAp :: Type -> (Type, [Type])
fromTAp t = f t [] where
    f (TAp a b) rs = f a (b:rs)
    f (TArrow a b) rs = f (tAp tArrow a) (b:rs)
    f t rs = (t,rs)

-- | Given a function type, return @(argtys, resty)@.
fromTArrow :: Type -> ([Type], Type)
fromTArrow t = f t [] where
    f (TArrow a b) rs = f b (a:rs)
    f t rs = (reverse rs,t)

-- | If the given type can be considered an application, return
-- @'Just' (function, argument)@, otherwise 'Nothing'.
splitTAp_maybe :: Type -> Maybe (Type, Type)
splitTAp_maybe (TAp a b) = Just (a, b)
splitTAp_maybe (TArrow a b) = Just (tAp tArrow a, b)
splitTAp_maybe t = Nothing

instance CanType MetaVar Kind where
    getType mv = metaKind mv

instance CanType Tycon Kind where
    getType (Tycon _ k) = k

instance CanType Tyvar Kind where
    getType = tyvarKind

instance CanType Type Kind where
  getType (TCon tc) = getType tc
  getType (TVar u)  = getType u
  getType typ@(TAp t _) = case (getType t) of
                     (Kfun _ k) -> k
                     x -> error $ "Representation.getType: kind error in: " ++ (show typ)
  getType (TArrow _l _r) = kindStar
  getType (TForAll _ (_ :=> t)) = getType t
  getType (TExists _ (_ :=> t)) = getType t
  getType (TMetaVar mv) = getType mv
  getType ta@TAssoc {} = getType (tassocToAp ta)

-- | Construct a tuple type
tTTuple :: [Type] -> Type
tTTuple ts | length ts < 2 = error "tTTuple"
tTTuple ts = foldl TAp (toTuple (length ts)) ts

-- | Construct an unboxed tuple type
tTTuple' :: [Type] -> Type
tTTuple' ts = foldl TAp (TCon $ Tycon (unboxedNameTuple TypeConstructor  n) (foldr Kfun kindUTuple $ replicate n kindStar)) ts where
    n = length ts

$(derive makeBinary ''Type)
$(derive makeBinary ''Tycon)
$(derive makeBinary ''Pred)
$(derive makeBinary ''Qual)