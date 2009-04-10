module FrontEnd.Tc.Kind(
    Kind(..),
    KBase(..),
    Kindvar(..),
    KindConstraint(..),
    kindCombine,
    kindStar,
    kindUTuple,
    kindFunRet,
    kindHash,
    kindArg,
    isSubsumedBy,
    unfoldKind
    ) where
import Data.DeriveTH
import Data.Derive.All
import Data.Monoid
import Control.Monad
import Data.IORef

import Data.Binary
import Doc.DocLike
import Doc.PPrint(pprint,pprintPrec,PPrint)
import Name.Name

{-

 KQuest = ?        star or hash or unboxed tuple
 KQuestQuest = ??  star or hash
 KUTuple = (#)     unboxed tuple
 Star    = *       boxed value
 KHash   = #       unboxed value
 Kfun    = (->)
 KNamed Foo = Foo  named kind

 we have the following subkinding going on

       ?
      / \
     ?? (#)
     /\
    *  #

in addition, user defined named kinds are allowed. these can only occur via
kind annotations, and only unify with themselves

-}

data KBase =
        Star
        | KHash
        | KUTuple
        | KQuestQuest
        | KQuest
        | KNamed Name
    deriving(Eq, Ord)   -- but we need them for kind inference

isSubsumedBy2 :: KBase -> KBase -> Bool
KNamed s1 `isSubsumedBy2` KNamed s2   = s1 == s2
_         `isSubsumedBy2` KQuest      = True
Star      `isSubsumedBy2` KQuestQuest = True
KHash     `isSubsumedBy2` KQuestQuest = True
k1        `isSubsumedBy2` k2          = k1 == k2

kindStar, kindHash, kindUTuple, kindFunRet, kindArg :: Kind
kindStar   = KBase Star
kindHash   = KBase KHash
kindUTuple = KBase KUTuple
kindFunRet = KBase KQuest
kindArg    = KBase KQuestQuest

data Kind  = KBase KBase
           | Kfun Kind Kind
           | KVar Kindvar               -- variables aren't really allowed in haskell in kinds
             deriving(Eq, Ord)   -- but we need them for kind inference

infixr `Kfun`

isSubsumedBy :: Kind -> Kind -> Bool
KBase kb    `isSubsumedBy` KBase kb'    = isSubsumedBy2 kb kb'
Kfun  k1 k2 `isSubsumedBy` Kfun k1' k2' = isSubsumedBy k1 k1' && isSubsumedBy k2 k2'
_           `isSubsumedBy` _            = False


kindCombine :: Monad m => Kind -> Kind -> m Kind
kindCombine x y = g x y where
    f x y | x == y = return x

    f KQuest x = fquest x
    f x  KQuest = fquest x
    f KQuestQuest x = fquest2 x
    f x  KQuestQuest = fquest2 x
    f x y = fail $ "kindCombine: " ++ show (x,y)
    fquest (KNamed n) = fail $ "Attempt to unify named kind" <+> tshow n <+> "with ?"
    fquest x = return x
    fquest2 (KNamed n) = fail $ "Attempt to unify named kind" <+> tshow n <+> "with ??"
    fquest2 KUTuple = fail $ "Attempt to unify unboxed tuple with ??"
    fquest2 KQuest = return KQuestQuest
    fquest2 x = return x
    g (KBase x) (KBase y) = f x y >>= return . KBase
    g (Kfun a b) (Kfun a' b') = return Kfun `ap` g a a' `ap` g b b'
    g x y = fail $ "kindCombine: " ++ show (x,y)

data KindConstraint
    = KindSimple     -- ^ @*@ | kindSimple -> kindSimple
    | KindQuest      -- ^ @?@, so @*@ or @(#)@ or @#@
    | KindQuestQuest -- ^ @??@, @*@ or @#@
    | KindStar       -- ^ must be @*@
    | KindAny        -- ^ may be anything
    deriving(Eq,Ord,Show)

-- note that named kinds are never inferred, so we don't need constraints
-- mentioning them.

instance Monoid KindConstraint where
    mempty = KindAny
    mappend a b | a == b = a
    mappend KindAny k = k
    mappend KindStar _ = KindStar
    mappend KindSimple KindQuest = KindStar
    mappend KindSimple KindQuestQuest = KindStar
    mappend KindQuest KindQuestQuest = KindQuestQuest
    mappend k1 k2 = mappend k2 k1

data Kindvar = Kindvar {
    kvarUniq :: !Int,
    kvarRef :: IORef (Maybe Kind),
    kvarConstraint :: KindConstraint
    }

instance Binary Kindvar where
    put _ = return ()
    get = return (error "Binary.Kindvar.get")

instance Eq Kindvar where
    a == b = kvarUniq a == kvarUniq b

instance Ord Kindvar where
    a `compare` b = kvarUniq a `compare` kvarUniq b

instance Show Kind where
    showsPrec n k = pprintPrec n k

instance Show Kindvar where
    showsPrec n k = pprintPrec n k


instance Show KBase where
    showsPrec _ Star    = showString "*"
    showsPrec _ KUTuple = showString "(#)"
    showsPrec _ KHash   = showString "#"
    showsPrec _ KQuest  = showString "?"
    showsPrec _ KQuestQuest = showString "??"
    showsPrec _ (KNamed n) = shows n

instance DocLike d => PPrint d KBase where
    pprint kb = text (show kb)

instance DocLike d => PPrint d Kind where
   pprintPrec _ (KBase b) = pprint b
   pprintPrec _ (KVar kindVar)   = pprint kindVar
   pprintPrec p k | p > 9 = parens $ pprint k
   pprintPrec _ (Kfun k1 k2) = pprintPrec 10 k1 <+> text "->" <+> pprint k2



instance DocLike d =>  PPrint d Kindvar where
   pprint Kindvar { kvarUniq = s } = text $ 'k':show s

--  * -> * == [*,*]
--  (*->*->*) -> * -> * == [(*->*->*), *, *]
unfoldKind :: Kind -> [Kind]
unfoldKind (Kfun k1 k2) = k1 : unfoldKind k2
unfoldKind v = [v]

$(derive makeBinary ''KBase)
$(derive makeBinary ''Kind)