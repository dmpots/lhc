module HashTable
    ( Hashable(..)
    , HashTable
    , empty
    , singleton
    , fromList
    , toList
    , insert
    , insertWith
    , lookup
    , findWithDefault
    ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Maybe
import Prelude hiding (lookup)

class Ord a => Hashable a where
    hash :: a -> Int
    hash _ = 0

newtype HashTable k v = HashTable (IntMap.IntMap (Map.Map k v))
    deriving (Eq, Ord)

instance (Show k, Show v, Hashable k) => Show (HashTable k v) where
    showsPrec n = showsPrec n . toMap

toMap :: Hashable k => HashTable k v -> Map.Map k v
toMap (HashTable imap)
    = Map.unions (IntMap.elems imap)

empty :: HashTable k v
empty = HashTable IntMap.empty

singleton :: (Hashable k) => k -> v -> HashTable k v
singleton k v = HashTable (IntMap.singleton (hash k) (Map.singleton k v))

fromList :: Hashable k => [(k,v)] -> HashTable k v
fromList lst
    = HashTable $ IntMap.fromListWith Map.union [ (hash k, Map.singleton k v) | (k,v) <- lst ]

toList :: Hashable k => HashTable k v -> [(k,v)]
toList = Map.toList . toMap

insert :: Hashable k => k -> v -> HashTable k v -> HashTable k v
insert k v (HashTable imap)
    = HashTable $ IntMap.insertWith Map.union (hash k) (Map.singleton k v) imap

insertWith :: Hashable k => (v -> v -> v) -> k -> v -> HashTable k v -> HashTable k v
insertWith merge k v (HashTable imap)
    = HashTable $ IntMap.insertWith (Map.unionWith merge) (hash k) (Map.singleton k v) imap

lookup :: Hashable k => k -> HashTable k v -> Maybe v
lookup k (HashTable imap)
    = do m <- IntMap.lookup (hash k) imap
         Map.lookup k m

findWithDefault :: Hashable k => v -> k -> HashTable k v -> v
findWithDefault def k ht
    = fromMaybe def (lookup k ht)
