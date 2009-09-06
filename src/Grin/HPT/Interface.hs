module Grin.HPT.Interface
    ( HeapAnalysis
    , Node
    , IsShared
    , Rhs(..)
    , isSubsetOf
    , mkHeapAnalysis
    , lookupHeap
    , lookupLhs
    , heapIsShared
    , hptIsShared
    , hptSetShared
    , hptAddBinding
    , rhsSize
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Grin.Types           ( Renamed(..), NodeType, uniqueId )
import Grin.HPT.Environment ( HeapPointer, Lhs(..), Node )

import Data.Monoid

import Grin.Stage2.Pretty (ppNodeType)
import Text.PrettyPrint.ANSI.Leijen

import Control.Parallel.Strategies

import qualified HashMap as HT

type IsShared = Bool

data Rhs
    = Empty
    | Base
    | Other { rhsTagged :: Map.Map Node [Rhs]
            , rhsVector :: [Rhs]
            , rhsHeap   :: Set.Set HeapPointer }
--    | Tagged (Map.Map Node [Rhs])
--    | Vector [Rhs]
--    | Heap (Set.Set HeapPointer)
    deriving (Eq)

instance Show Rhs where
    showsPrec _ = displayS . renderPretty 1 200 . ppRhs

ppRhs Empty          = text "Empty"
ppRhs Base           = text "Base"
ppRhs Other{}        = text "Other"
--ppRhs (Tagged nodes) = list [ (ppNodeType nt missing tag) <+> list (map ppRhs args) | ((tag, nt, missing), args) <- Map.toList nodes ]
--ppRhs (Vector args)  = list (map ppRhs args)
--ppRhs (Heap hps)     = list (map int (Set.toList hps))

instance Monoid Rhs where
    mempty = Empty
    mappend = joinRhs

joinRhs :: Rhs -> Rhs -> Rhs
joinRhs Empty rhs                         = rhs
joinRhs rhs Empty                         = rhs
joinRhs Base Base                         = Base
joinRhs (Other t1 v1 h1) (Other t2 v2 h2) = Other (Map.unionWith zipJoin t1 t2) (zipJoin v1 v2) (Set.union h1 h2)
joinRhs rhs Base                          = rhs
joinRhs Base rhs                          = rhs
{-
joinRhs (Tagged nodes1) (Tagged nodes2)   = Tagged (Map.unionWith zipJoin nodes1 nodes2)
joinRhs (Vector args1) (Vector args2)     = Vector (zipJoin args1 args2)
joinRhs (Heap hp1) (Heap hp2)             = Heap (Set.union hp1 hp2)
joinRhs left right                        = error $ "Unmatched rhs values: " ++ show (left,right)
-}

isSubsetOf :: Rhs -> Rhs -> Bool
lRhs `isSubsetOf` rRhs
    = worker lRhs rRhs
    where worker Empty y  = True
          worker x Empty  = False
          worker _ Base   = True
          worker Base _   = False
          worker (Other t1 v1 h1) (Other t2 v2 h2)
              = Map.isSubmapOfBy (\a b -> and (zipWith isSubsetOf a b)) t1 t2 &&
                and (zipWith isSubsetOf v1 v2) &&
                Set.isSubsetOf h1 h2
{-
          worker (Tagged nodes1) (Tagged nodes2)
              = Map.isSubmapOfBy (\a b -> and (zipWith isSubsetOf a b)) nodes1 nodes2
          worker (Vector args1) (Vector args2)
              = and (zipWith isSubsetOf args1 args2)
          worker (Heap hp1) (Heap hp2)
              = Set.isSubsetOf hp1 hp2
          worker Base Base = True
          worker _ _ = False -- should be an error.
-}


zipJoin :: Monoid a => [a] -> [a] -> [a]
zipJoin [] []         = []
zipJoin [] lst        = zipWith mappend (repeat mempty) lst
zipJoin lst []        = zipWith mappend lst (repeat mempty)
zipJoin (x:xs) (y:ys) = mappend x y : zipJoin xs ys


instance HT.Hashable Lhs where
    hash (VarEntry var) = HT.hash var
    hash (HeapEntry hp) = hp

data HeapAnalysis
    = HeapAnalysis { hptBindings   :: HT.HashTable Lhs Rhs
                   , hptSharingMap :: Map.Map Lhs IsShared
                   }
    deriving (Eq)

mkHeapAnalysis :: Map.Map Lhs Rhs -> Map.Map Lhs IsShared -> HeapAnalysis
mkHeapAnalysis binds smap
    = HeapAnalysis { hptBindings   = HT.fromList (Map.toList binds)
                   , hptSharingMap = smap
                   }

lookupLhs :: Lhs -> HeapAnalysis -> Rhs
lookupLhs lhs hpt
    = HT.findWithDefault Empty lhs (hptBindings hpt)


rhsSize :: Rhs -> Int
rhsSize Empty = 0
rhsSize Base = 1
rhsSize (Other t v h)
    = maximum [ (1 + maximum (0:map length (Map.elems t)))
              , length v ]
{-
rhsSize (Tagged nodes) = 1 + maximum (0:map length (Map.elems nodes))
rhsSize (Vector args) = length args
rhsSize Heap{} = 1
-}

lookupHeap :: Renamed -> HeapAnalysis -> Rhs
lookupHeap var hpt
    = case HT.lookup (VarEntry var) (hptBindings hpt) of
        Just (Other{rhsHeap=hp}) -> mconcat [ HT.findWithDefault (errMsg pointer) (HeapEntry pointer) (hptBindings hpt) | pointer <- Set.toList hp ]
        Just Empty     -> Empty
        Just rhs       -> error $ "Grin.HPT.Interface.lookupHeap: Invalid rhs: " ++ show (var, rhs)
        Nothing        -> error $ "Grin.HPT.Interface.lookupHeap: Couldn't find lhs: " ++ show var
    where errMsg p = error $ "Grin.HPT.Interface.lookupHeap: Heap value not found: " ++ show p

heapIsShared :: Renamed -> HeapAnalysis -> IsShared
heapIsShared var hpt
    = case HT.lookup (VarEntry var) (hptBindings hpt) of
        Just (Other{rhsHeap=hp}) -> or [ Map.findWithDefault False (HeapEntry pointer) (hptSharingMap hpt) | pointer <- Set.toList hp ]
        Just Empty     -> False
        Just rhs       -> error $ "Grin.HPT.Interface.heapIsShared: Invalid rhs: " ++ show (var, rhs)
        Nothing        -> error $ "Grin.HPT.Interface.heapIsShared: Couldn't find lhs: " ++ show var

hptIsShared :: Lhs -> HeapAnalysis -> IsShared
hptIsShared lhs hpt
    = Map.findWithDefault False lhs (hptSharingMap hpt)

hptSetShared :: Lhs -> HeapAnalysis -> HeapAnalysis
hptSetShared lhs hpt
    = hpt { hptSharingMap = Map.insert lhs True (hptSharingMap hpt) }

hptAddBinding :: Lhs -> Rhs -> HeapAnalysis -> HeapAnalysis
hptAddBinding lhs rhs hpt
    = hpt { hptBindings = HT.insertWith mappend lhs rhs (hptBindings hpt) }

