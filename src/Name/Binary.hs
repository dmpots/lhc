module Name.Binary() where

import Maybe
import Data.Monoid

import Data.Binary
import Name.Id
import Name.Name

instance Binary IdSet where
    put ids = do
        put [ idToInt id | id <- idSetToList ids, isNothing (fromId id)]
        put [ n | id <- idSetToList ids, n <- fromId id]
    get = do
        (idl:: [Int])   <- get
        (ndl:: [Name]) <- get
        return (idSetFromDistinctAscList (map unnamed idl) `mappend` idSetFromList (map toId ndl))


instance Binary a => Binary (IdMap a) where
    put ids = do
        put [ (idToInt id,v) | (id,v) <- idMapToList ids, isNothing (fromId id)]
        put [ (n,v) | (id,v) <- idMapToList ids, n <- fromId id]
    get = do
        idl <- get
        ndl <- get
        return (idMapFromDistinctAscList [ (unnamed x, y) | (x,y) <- idl ] `mappend` idMapFromList [ (toId n,v) | (n,v) <- ndl ])

