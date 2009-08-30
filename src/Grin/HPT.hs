module Grin.HPT
    ( analyze
    , lower
    , module Grin.HPT.Interface
    , Lhs (..)
    ) where

import Grin.Types               ( Grin )

import Grin.HPT.Environment     ( mkEnvironment, Lhs(..) )
import Grin.HPT.Solve           ( solve )
import Grin.HPT.Lower           ( lower )
import Grin.HPT.Interface       ( HeapAnalysis, joinRhs, lookupLhs, rhsSize
                                , lookupHeap, heapIsShared, lhsIsShared
                                , Rhs(..) )

analyze :: Grin -> (Int, HeapAnalysis)
analyze = solve . mkEnvironment

-- nodeSize :: Renamed -> Int
-- heapPointers :: Renamed -> [HeapPointer]
-- heapNodeValues :: HeapPointer -> [Node set]
