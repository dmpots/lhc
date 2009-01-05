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
    unnamed,
    idSetFromDistinctAscList,
    idMapFromList,
    idMapFromDistinctAscList,
    idSetToList,
    idMapToList,
    isEmptyId,
    emptyId,
    newIds,
    newId,
    runIdNameT',
    runIdNameT,
    toId,
    fromId
    )where

import Control.Monad.State
import Control.Monad.Reader
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Data.Typeable
import System.Random
import Data.Bits
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

-- TODO - make this a newtype
data Id = Id Int
          deriving (Read,Show,Eq,Ord)
-- data Id = Etherial Int | NoBind | Named Name | Unnamed Int

instance DocLike d => PPrint d Id where
    pprint (Id i) = pprint i

-- IdSet

toId :: Name -> Id
toId x = Id (fromAtom (toAtom x))

fromId :: Monad m => Id -> m Name
fromId (Id i) = case intToAtom i of
    Just a -> return $ Name a
    Nothing -> fail $ "Name.fromId: not a name " ++ show i


newtype IdSet = IdSet (Set.Set Id)
    deriving(Typeable,Monoid,HasSize,SetLike,BuildSet Id,ModifySet Id,IsEmpty,Eq,Ord)


idSetToList :: IdSet -> [Id]
idSetToList (IdSet is) = Set.toList is

idMapToList :: IdMap a -> [(Id,a)]
idMapToList (IdMap is) = Map.toList is

idToInt :: Id -> Int
idToInt (Id i) = i

mapMaybeIdMap :: (a -> Maybe b) -> IdMap a -> IdMap b
mapMaybeIdMap fn (IdMap m) = IdMap (Map.mapMaybe fn m)


-- IdMap

newtype IdMap a = IdMap (Map.Map Id a)
    deriving(Typeable,Monoid,HasSize,SetLike,BuildSet (Id,a),MapLike Id a,Functor,Traversable,Foldable,IsEmpty,Eq,Ord)


idSetToIdMap :: (Id -> a) -> IdSet -> IdMap a
idSetToIdMap f (IdSet is) = IdMap $ Map.fromDistinctAscList [ (x,f x) |  x <- Set.toAscList is]

idMapToIdSet :: IdMap a -> IdSet
idMapToIdSet (IdMap im) = IdSet $ (Map.keysSet im)


-- | Name monad transformer.
newtype IdNameT m a = IdNameT (StateT (IdSet, IdSet) m a)
    deriving(Monad, MonadTrans, Functor, MonadFix, MonadPlus, MonadIO)

instance (MonadReader r m) => MonadReader r (IdNameT m) where
	ask       = lift ask
	local f (IdNameT m) = IdNameT $ local f m

-- | Get bound and used names
idNameBoundNames :: Monad m => IdNameT m IdSet
idNameBoundNames = IdNameT $ do
    (_used,bound) <- get
    return bound
idNameUsedNames :: Monad m => IdNameT m IdSet
idNameUsedNames = IdNameT $  do
    (used,_bound) <- get
    return used

-- | Run the name monad transformer.
runIdNameT :: (Monad m) => IdNameT m a -> m a
runIdNameT (IdNameT x) = liftM fst $ runStateT x (mempty,mempty)

runIdNameT' :: (Monad m) => IdNameT m a -> m (a,IdSet)
runIdNameT' (IdNameT x) = do
    (r,(used,bound)) <- runStateT x (mempty,mempty)
    return (r,bound)

fromIdNameT (IdNameT x) = x

instance GenName Id where
    genNames i = map unnamed [st, st + 2 ..]  where
        st = abs i + 2 + abs i `mod` 2

instance Monad m => NameMonad Id (IdNameT m) where
    addNames ns = IdNameT $ do
        modify (\ (used,bound) -> (fromList ns `union` used, bound) )
    addBoundNames ns = IdNameT $ do
        let nset = fromList ns
        modify (\ (used,bound) -> (nset `union` used, nset `union` bound) )
    uniqueName n = IdNameT $ do
        (used,bound) <- get
        if n `member` bound then fromIdNameT newName else put (insert n used,insert n bound) >> return n
    newNameFrom vs = IdNameT $ do
        (used,bound) <- get
        let f (x:xs)
                | x `member` used = f xs
                | otherwise = x
            f [] = error "newNameFrom: finite list!"
            nn = f vs
        put (insert nn used, insert nn bound)
        return nn
    newName  = IdNameT $ do
        (used,bound) <- get
        let genNames i = map unnamed [st, st + 2 ..]  where
                st = abs i + 2 + abs i `mod` 2
        fromIdNameT $ newNameFrom  (genNames (size used + size bound))

addNamesIdSet nset = IdNameT $ do
    modify (\ (used,bound) -> (nset `union` used, bound) )
addBoundNamesIdSet nset = IdNameT $ do
    modify (\ (used,bound) -> (nset `union` used, nset `union` bound) )

addBoundNamesIdMap nmap = IdNameT $ do
    modify (\ (used,bound) -> (nset `union` used, nset `union` bound) ) where
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
etherialIds = map unnamed [-2, -4 ..  ]

isEtherialId (Id id) = id < 0

isInvalidId (Id id) = id <= 0

isEmptyId :: Id -> Bool
isEmptyId (Id 0) = True
isEmptyId _ = False

emptyId :: Id
emptyId = Id 0

unnamed :: Int -> Id
unnamed = Id

-- | find some temporary ids that are not members of the set,
-- useful for generating a small number of local unique names.

newIds :: IdSet -> [Id]
newIds ids = [ unnamed i | i <- [s, s + 2 ..] , unnamed i `notMember` ids ] where
    s = 2 + (2 * size ids)


newId :: Int           -- ^ a seed value, useful for speeding up finding a unique id
      -> (Id -> Bool)  -- ^ whether an Id is acceptable
      -> Id            -- ^ your new Id
newId seed check = head $ filter check ls where
    ls = map unnamed $ map mask $ randoms (mkStdGen seed)
    mask x = x .&. 0x0FFFFFFE




