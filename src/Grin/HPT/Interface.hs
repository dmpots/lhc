module Grin.HPT.Interface where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Grin.Types           ( Renamed, NodeType )
import Grin.HPT.Environment ( HeapPointer, Lhs(..) )

type Node = (Renamed, NodeType, Int) -- Name, node type, missing arguments.
type IsShared = Bool

data Rhs
    = Empty
    | Base
    | Tagged (Map.Map Node [Rhs])
    | Vector [Rhs]
    | Heap (Set.Set HeapPointer)
    deriving (Show)

joinRhs :: Rhs -> Rhs -> Rhs
joinRhs Empty rhs                         = rhs
joinRhs rhs Empty                         = rhs
joinRhs Base Base                         = Base
joinRhs (Tagged nodes1) (Tagged nodes2)   = Tagged (Map.unionWith (zipWith joinRhs) nodes1 nodes2)
joinRhs (Vector args1) (Vector args2)     = Vector (zipWith joinRhs args1 args2)
joinRhs (Heap hp1) (Heap hp2)             = Heap (Set.union hp1 hp2)
joinRhs left right                        = error $ "Unmatched rhs values: " ++ show (left,right)

data HeapAnalysis
    = HeapAnalysis (Map.Map Lhs Rhs) (Map.Map Lhs IsShared)


lookupLhs :: Lhs -> HeapAnalysis -> Rhs
lookupLhs lhs (HeapAnalysis binds _smap)
    = Map.findWithDefault Empty lhs binds

rhsSize :: Rhs -> Int
rhsSize Empty = 0
rhsSize Base = 1
rhsSize (Tagged nodes) = 1 + maximum (0:map length (Map.elems nodes))
rhsSize (Vector args) = length args
rhsSize Heap{} = 1

lookupHeap :: Lhs -> HeapAnalysis -> Rhs
lookupHeap lhs (HeapAnalysis binds _smap)
    = case Map.lookup lhs binds of
        Just (Heap hp) -> foldr joinRhs Empty [ Map.findWithDefault (errMsg pointer) (HeapEntry pointer) binds | pointer <- Set.toList hp ]
        Just Empty     -> Empty
        Just rhs       -> error $ "Grin.HPT.Interface.lookupHeap: Invalid rhs: " ++ show (lhs, rhs)
        Nothing        -> error $ "Grin.HPT.Interface.lookupHeap: Couldn't find lhs: " ++ show lhs
    where errMsg p = error $ "Grin.HPT.Interface.lookupHeap: Heap value not found: " ++ show p

heapIsShared :: Lhs -> HeapAnalysis -> IsShared
heapIsShared lhs (HeapAnalysis binds smap)
    = case Map.lookup lhs binds of
        Just (Heap hp) -> or [ Map.findWithDefault False (HeapEntry pointer) smap | pointer <- Set.toList hp ]
        Just Empty     -> False
        Just rhs       -> error $ "Grin.HPT.Interface.heapIsShared: Invalid rhs: " ++ show (lhs, rhs)
        Nothing        -> error $ "Grin.HPT.Interface.heapIsShared: Couldn't find lhs: " ++ show lhs

lhsIsShared :: Lhs -> HeapAnalysis -> IsShared
lhsIsShared lhs (HeapAnalysis _binds smap)
    = Map.findWithDefault False lhs smap
