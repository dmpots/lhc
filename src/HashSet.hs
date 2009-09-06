module HashSet
    ( Hashable(..)
    , HashSet
    , empty
    , singleton
    , fromList
    , toList
    , union
    , insert
    , delete
    , member
    , difference
    , isSubsetOf
    , approxSuperset
    ) where

import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.Maybe
import Prelude hiding (lookup)

import HashMap (Hashable(..), HashMap)
import qualified HashMap

newtype HashSet v = HashSet (IntMap.IntMap (Set.Set v))
    deriving (Eq, Ord)

instance (Show v, Hashable v) => Show (HashSet v) where
    showsPrec n = showsPrec n . toSet

toSet :: Hashable v => HashSet v -> Set.Set v
toSet (HashSet imap)
    = Set.unions (IntMap.elems imap)

empty :: HashSet v
empty = HashSet IntMap.empty

singleton :: (Hashable v) =>  v -> HashSet v
singleton v = HashSet (IntMap.singleton (hash v) (Set.singleton v))

fromList :: Hashable v => [v] -> HashSet v
fromList lst
    = HashSet $ IntMap.fromListWith Set.union [ (hash v, Set.singleton v) | v <- lst ]

toList :: Hashable v => HashSet v -> [v]
toList = Set.toList . toSet

union :: Hashable v => HashSet v -> HashSet v -> HashSet v
union (HashSet a) (HashSet b) = HashSet (IntMap.unionWith (Set.union) a b)

insert :: Hashable v => v -> HashSet v -> HashSet v
insert v (HashSet imap)
    = HashSet $ IntMap.insertWith Set.union (hash v) (Set.singleton v) imap

delete :: Hashable v => v -> HashSet v -> HashSet v
delete v (HashSet imap)
    = HashSet $ IntMap.update fn (hash v) imap
    where fn s = let s' = Set.delete v s
                 in if Set.null s' then Nothing else Just s'

member :: Hashable v => v -> HashSet v -> Bool
member v (HashSet imap)
    = case IntMap.lookup (hash v) imap of
        Nothing  -> False
        Just set -> v `Set.member` set

difference :: Hashable v => HashSet v -> HashSet v -> HashSet v
difference (HashSet a) (HashSet b)
    = HashSet $ IntMap.differenceWith worker a b
    where worker a' b' = let d = Set.difference a' b' in
                         if Set.null d then Nothing else Just d

isSubsetOf :: Hashable v => HashSet v -> HashSet v -> Bool
isSubsetOf (HashSet a) (HashSet b)
    = IntMap.isSubmapOfBy (Set.isSubsetOf) a b

approxSuperset :: Hashable v => HashSet v -> HashMap v k -> Bool
approxSuperset (HashSet a) b
    = let m = HashMap.unpack b
          a' = IntMap.difference a m
      in IntMap.size a' == IntMap.size a
