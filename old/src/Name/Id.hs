module Name.Id(
    Id(),
    IdMap(),
    IdNameT(),
    IdSet(),
    addBoundNamesIdMap,
    addBoundNamesIdSet,
    addNamesIdSet,
    idMapToIdSet,
    idNameBoundNames,
    idNameUsedNames,
    etherialIds,
    isEtherialId,
    isInvalidId,
    idSetToIdMap,
    mapMaybeIdMap,
    idSetFromList,
    idToInt,
    idIsNamed,
    anonymous,
    idSetFromDistinctAscList,
    idMapFromList,
    idMapFromDistinctAscList,
    idSetToList,
    idMapToList,
    isEmptyId,
    emptyId,
    sillyId,
    newIds,
    newId,
    runIdNameT',
    runIdNameT,
    toId,
    fromId
    )where

import qualified Control.Monad.State as State
import Control.Monad.Reader
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Data.Typeable
import System.Random
import Data.Bits
import Data.Maybe (isJust)
import qualified Data.Map  as Map
import qualified Data.Set as Set

import StringTable.Atom
import Util.HasSize
import Util.Inst()
import Util.NameMonad
import Util.SetLike as S
import Name.Name
import Doc.PPrint
import Doc.DocLike

import Data.DeriveTH
import Data.Derive.All
import Data.Binary

data Id = Empty                -- ^ Empty binding. Like '\ _ -> ...'.
        | Etherial Int         -- ^ Special ids used for typechecking. They should never be exposed to the user.
        | Anonymous Int        -- ^ Anonymous id created by the compiler.
        | Named Name           -- ^ Named id created mostly by the user.
          deriving (Eq,Ord)

$(derive makeBinary ''Id)

instance Show Id where
    showsPrec n (Anonymous i) = showsPrec n i
    showsPrec n (Named x) = showsPrec n x
    showsPrec n (Etherial i) = showsPrec n (-i)
    showsPrec n Empty = showsPrec n "_"

instance DocLike d => PPrint d Id where
    pprint (Anonymous i) = pprint i
    pprint (Named n) = pprint n
    pprint (Etherial i) = pprint (-i)
    pprint Empty = pprint "_"

-- IdSet

toId :: Name -> Id
toId x = Named x
--toId x = Id (fromAtom (toAtom x))

fromId :: Monad m => Id -> m Name
fromId (Named n) = return n
fromId i = fail $ "Name.fromId: not a name " ++ show i


newtype IdSet = IdSet (Set.Set Id)
    deriving(Typeable,Monoid,HasSize,SetLike,BuildSet Id,ModifySet Id,IsEmpty,Eq,Ord,Binary)


idSetToList :: IdSet -> [Id]
idSetToList (IdSet is) = Set.toList is

idMapToList :: IdMap a -> [(Id,a)]
idMapToList (IdMap is) = Map.toList is

idToInt :: Id -> Int
idToInt (Anonymous i) = i
idToInt (Named n) = fromAtom (toAtom n)
idToInt (Etherial i) = -i
idToInt Empty = 0

idIsNamed :: Id -> Bool
idIsNamed = isJust . fromId

mapMaybeIdMap :: (a -> Maybe b) -> IdMap a -> IdMap b
mapMaybeIdMap fn (IdMap m) = IdMap (Map.mapMaybe fn m)


-- IdMap

newtype IdMap a = IdMap (Map.Map Id a)
    deriving(Typeable,Monoid,HasSize,SetLike,BuildSet (Id,a),MapLike Id a,Functor,Traversable,Foldable,IsEmpty,Eq,Ord,Binary)


idSetToIdMap :: (Id -> a) -> IdSet -> IdMap a
idSetToIdMap f (IdSet is) = IdMap $ Map.fromDistinctAscList [ (x,f x) |  x <- Set.toAscList is]

idMapToIdSet :: IdMap a -> IdSet
idMapToIdSet (IdMap im) = IdSet $ (Map.keysSet im)


-- | Name monad transformer.
newtype IdNameT m a = IdNameT (State.StateT (IdSet, IdSet) m a)
    deriving(Monad, MonadTrans, Functor, MonadFix, MonadPlus, MonadIO)

instance (MonadReader r m) => MonadReader r (IdNameT m) where
	ask       = lift ask
	local f (IdNameT m) = IdNameT $ local f m

-- | Get bound and used names
idNameBoundNames :: Monad m => IdNameT m IdSet
idNameBoundNames = IdNameT $ do
    (_used,bound) <- State.get
    return bound
idNameUsedNames :: Monad m => IdNameT m IdSet
idNameUsedNames = IdNameT $  do
    (used,_bound) <- State.get
    return used

-- | Run the name monad transformer.
runIdNameT :: (Monad m) => IdNameT m a -> m a
runIdNameT (IdNameT x) = liftM fst $ State.runStateT x (mempty,mempty)

runIdNameT' :: (Monad m) => IdNameT m a -> m (a,IdSet)
runIdNameT' (IdNameT x) = do
    (r,(used,bound)) <- State.runStateT x (mempty,mempty)
    return (r,bound)

fromIdNameT :: IdNameT m a -> State.StateT (IdSet, IdSet) m a
fromIdNameT (IdNameT x) = x

instance GenName Id where
    genNames i = map anonymous [st, st + 2 ..]  where
        st = abs i + 2 + abs i `mod` 2

instance Monad m => NameMonad Id (IdNameT m) where
    addNames ns = IdNameT $ do
        State.modify (\ (used,bound) -> (fromList ns `union` used, bound) )
    addBoundNames ns = IdNameT $ do
        let nset = fromList ns
        State.modify (\ (used,bound) -> (nset `union` used, nset `union` bound) )
    uniqueName n = IdNameT $ do
        (used,bound) <- State.get
        if n `member` bound then fromIdNameT newName else State.put (insert n used,insert n bound) >> return n
    newNameFrom vs = IdNameT $ do
        (used,bound) <- State.get
        let f (x:xs)
                | x `member` used = f xs
                | otherwise = x
            f [] = error "newNameFrom: finite list!"
            nn = f vs
        State.put (insert nn used, insert nn bound)
        return nn
    newName  = IdNameT $ do
        (used,bound) <- State.get
        let genNames i = map anonymous [st, st + 2 ..]  where
                st = abs i + 2 + abs i `mod` 2
        fromIdNameT $ newNameFrom  (genNames (size used + size bound))

addNamesIdSet :: Monad m => IdSet -> IdNameT m ()
addNamesIdSet nset = IdNameT $ do
    State.modify (\ (used,bound) -> (nset `union` used, bound) )
addBoundNamesIdSet :: Monad m => IdSet -> IdNameT m ()
addBoundNamesIdSet nset = IdNameT $ do
    State.modify (\ (used,bound) -> (nset `union` used, nset `union` bound) )

addBoundNamesIdMap :: Monad m => IdMap a -> IdNameT m ()
addBoundNamesIdMap nmap = IdNameT $ do
    State.modify (\ (used,bound) -> (nset `union` used, nset `union` bound) ) where
        nset = idMapToIdSet nmap

idSetFromDistinctAscList :: [Id] -> IdSet
idSetFromDistinctAscList ids = IdSet (Set.fromDistinctAscList ids)

idSetFromList :: [Id] -> IdSet
idSetFromList ids = IdSet (Set.fromList ids)

idMapFromList :: [(Id,a)] -> IdMap a
idMapFromList ids = IdMap (Map.fromList ids)

idMapFromDistinctAscList :: [(Id,a)] -> IdMap a
idMapFromDistinctAscList ids = IdMap (Map.fromDistinctAscList ids)


instance Show IdSet where
    showsPrec n is = showsPrec n $ map f (idSetToList is) where
        f n =  maybe (toAtom ('x':show n)) (toAtom . show) (fromId n)

instance Show v => Show (IdMap v) where
    showsPrec n is = showsPrec n $ map f (idMapToList is) where
        f (n,v) =  (maybe (toAtom ('x':show n)) (toAtom . show) (fromId n),v)

-- Id types
-- odd - an atom
-- 0 - special, indicating lack of binding
-- negative - etherial id, used as placeholder within algorithms
-- positive and even - arbitrary numbers.

etherialIds :: [Id]
etherialIds = map Etherial [2, 3 ..  ]

isEtherialId :: Id -> Bool
isEtherialId Etherial{} = True
isEtherialId _ = False

isInvalidId :: Id -> Bool
isInvalidId Etherial{} = True
isInvalidId Empty = True
isInvalidId _ = False

isEmptyId :: Id -> Bool
isEmptyId Empty = True
isEmptyId _ = False

emptyId :: Id
emptyId = Empty

anonymous :: Int -> Id
anonymous x | x <= 0    = error "Anonymous variables must be positive numbers."
            | otherwise = Anonymous x

sillyId :: Id
sillyId = Etherial 1

-- | find some temporary ids that are not members of the set,
-- useful for generating a small number of local unique names.

newIds :: IdSet -> [Id]
newIds ids = [ anonymous i | i <- [s ..] , anonymous i `notMember` ids ] where
    s = 1 + (size ids)


newId :: Int           -- ^ a seed value, useful for speeding up finding a unique id
      -> (Id -> Bool)  -- ^ whether an Id is acceptable
      -> Id            -- ^ your new Id
newId seed check = head $ filter check ls where
    ls = map anonymous $ map mask $ randoms (mkStdGen seed)
    mask x = x .&. 0x0FFFFFFE




