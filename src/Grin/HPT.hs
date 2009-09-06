module Grin.HPT
    ( analyze
    , lower
    , module Grin.HPT.Interface
    , Lhs (..)
    ) where

import Grin.Types               ( Grin )

import Grin.HPT.Environment     ( mkEnvironment, Lhs(..) )
import Grin.HPT.Solve           ( )
import Grin.HPT.QuickSolve      ( solve )
import Grin.HPT.Lower           ( lower )
import Grin.HPT.Interface

analyze :: Grin -> ([Int], HeapAnalysis)
analyze = solve . mkEnvironment

