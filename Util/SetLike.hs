module Util.SetLike(
    EnumSet(),
    (\\),
    notMember,
    union,
    unions,
    minsert,
    msingleton,
    SetLike(..),
    ModifySet(..),
    MapLike(..),
    BuildSet(..)
    ) where


import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Monoid
import Data.Typeable

import Util.HasSize

infixl 9 \\ --

m1 \\ m2 = difference m1 m2

class (Monoid s,HasSize s) => SetLike s where
    difference :: s -> s -> s
    intersection :: s -> s -> s
    disjoint :: s -> s -> Bool
    isSubsetOf :: s -> s -> Bool
    null :: s -> Bool

    disjoint x y = Util.SetLike.null (x `intersection` y)
    isSubsetOf x y = size x <= size y && (size (x `intersection` y) == size x)


-- you can't pull values out of the set with this, as it might store the
-- essence of a data type

class SetLike s => BuildSet t s | s -> t where
    fromList :: [t] -> s
    fromDistinctAscList :: [t] -> s
    insert :: t -> s -> s
    singleton :: t -> s

    singleton t = fromDistinctAscList [t]
    fromDistinctAscList = fromList

class BuildSet t s => ModifySet t s | s -> t where
    delete :: t -> s -> s
    member :: t -> s -> Bool

notMember x t = not $ member x t

union :: SetLike a => a -> a -> a
union = mappend

unions :: SetLike a => [a] -> a
unions = mconcat

--  int set

instance SetLike IS.IntSet where
    difference = IS.difference
    intersection = IS.intersection
    isSubsetOf = IS.isSubsetOf
    null = IS.null

instance BuildSet Int IS.IntSet where
    fromList xs = IS.fromList xs
    fromDistinctAscList xs = IS.fromDistinctAscList xs
    insert x s = IS.insert x s
    singleton x = IS.singleton x

instance ModifySet Int IS.IntSet where
    delete x s = IS.delete x s
    member x s = IS.member x s

-- normal set

instance Ord a => SetLike (S.Set a) where
    difference = S.difference
    intersection = S.intersection
    isSubsetOf = S.isSubsetOf
    null = S.null

instance Ord a => BuildSet a (S.Set a) where
    fromList xs = S.fromList xs
    fromDistinctAscList xs = S.fromDistinctAscList xs
    insert x s = S.insert x s
    singleton x = S.singleton x

instance Ord a => ModifySet a (S.Set a) where
    member x s = S.member x s
    delete x s = S.delete x s

-- maps

instance Ord a => SetLike (IM.IntMap a) where    -- SIC
    difference = IM.difference
    intersection = IM.intersection
    null = IM.null


instance Ord a => BuildSet (Int,a) (IM.IntMap a) where
    fromList xs = IM.fromList xs
    fromDistinctAscList xs = IM.fromDistinctAscList xs
    insert (k,v) s = IM.insert k v s
    singleton (k,v) = IM.singleton k v


instance Ord a => SetLike (M.Map a b) where
    difference = M.difference
    intersection = M.intersection
    null = M.null

instance Ord a => BuildSet (a,b) (M.Map a b) where
    fromList xs = M.fromList xs
    fromDistinctAscList xs = M.fromDistinctAscList xs
    insert (k,v) s = M.insert k v s
    singleton (k,v) = M.singleton k v

minsert :: BuildSet (k,v) s => k -> v -> s -> s
minsert k v s = insert (k,v) s

msingleton :: BuildSet (k,v) s => k -> v -> s
msingleton k v = singleton (k,v)


class SetLike m => MapLike k v m | m -> k v where
    mdelete :: k -> m -> m
    mmember :: k -> m -> Bool
    mlookup :: Monad g => k -> m -> g v
    melems :: m -> [v]

instance Ord a => MapLike Int a (IM.IntMap a) where
    mdelete = IM.delete
    mmember = IM.member
    mlookup k m = case IM.lookup k m of
        Nothing -> fail $ "IntMap: can't find " ++ show k
        Just x -> return x
    melems = IM.elems

instance Ord k => MapLike k v (M.Map k v) where
    mdelete = M.delete
    mmember = M.member
    mlookup = M.lookup
    melems = M.elems


-- EnumSet

newtype EnumSet a = EnumSet IS.IntSet
    deriving(Typeable,Monoid,SetLike,HasSize,Eq,Ord)

instance Enum a => BuildSet a (EnumSet a) where
    fromList xs = EnumSet $ IS.fromList (map fromEnum xs)
    fromDistinctAscList xs = EnumSet $ IS.fromDistinctAscList (map fromEnum xs)
    insert x (EnumSet s) = EnumSet $ IS.insert (fromEnum x) s
    singleton x = EnumSet $ IS.singleton (fromEnum x)

instance Enum a => ModifySet a (EnumSet a) where
    member x (EnumSet s) = IS.member (fromEnum x) s
    delete x (EnumSet s) = EnumSet $ IS.delete (fromEnum x) s

