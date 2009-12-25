{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HashMap
    ( Hashable(..)
    , HashMap
    , unpack
    , empty
    , singleton
    , fromList
    , toList
    , insert
    , insertWith
    , lookup
    , member
    , delete
    , findWithDefault
    ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Maybe
import Prelude hiding (lookup)
import Control.Parallel.Strategies


class Ord a => Hashable a where
    hash :: a -> Int
    hash _ = 0

newtype HashMap k v = HashMap (IntMap.IntMap (Map.Map k v))
    deriving (Eq, Ord, NFData)

instance (Show k, Show v, Hashable k) => Show (HashMap k v) where
    showsPrec n = showsPrec n . toMap

unpack :: HashMap k v -> IntMap.IntMap (Map.Map k v)
unpack (HashMap a) = a

toMap :: Hashable k => HashMap k v -> Map.Map k v
toMap (HashMap imap)
    = Map.unions (IntMap.elems imap)

empty :: HashMap k v
empty = HashMap IntMap.empty

singleton :: (Hashable k) => k -> v -> HashMap k v
singleton k v = HashMap (IntMap.singleton (hash k) (Map.singleton k v))

fromList :: Hashable k => [(k,v)] -> HashMap k v
fromList lst
    = HashMap $ IntMap.fromListWith Map.union [ (hash k, Map.singleton k v) | (k,v) <- lst ]

toList :: Hashable k => HashMap k v -> [(k,v)]
toList (HashMap imap) = concatMap Map.toList (IntMap.elems imap)
--toList = Map.toList . toMap

toAscList :: Hashable k => HashMap k v -> [(k,v)]
toAscList = Map.toAscList . toMap

insert :: Hashable k => k -> v -> HashMap k v -> HashMap k v
insert k v (HashMap imap)
    = HashMap $ IntMap.insertWith Map.union (hash k) (Map.singleton k v) imap

insertWith :: Hashable k => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
insertWith merge k v (HashMap imap)
    = HashMap $ IntMap.insertWith (Map.unionWith merge) (hash k) (Map.singleton k v) imap

lookup :: Hashable k => k -> HashMap k v -> Maybe v
lookup k (HashMap imap)
    = do m <- IntMap.lookup (hash k) imap
         Map.lookup k m

member :: Hashable k => k -> HashMap k v -> Bool
member k (HashMap imap)
    = case IntMap.lookup (hash k) imap of
        Nothing -> False
        Just m  -> Map.member k m

delete :: Hashable k => k -> HashMap k v -> HashMap k v
delete k (HashMap imap)
    = HashMap $ IntMap.update fn (hash k) imap
    where fn m = let m' = Map.delete k m
                 in if Map.null m' then Nothing else Just m'

findWithDefault :: Hashable k => v -> k -> HashMap k v -> v
findWithDefault def k ht
    = fromMaybe def (lookup k ht)
