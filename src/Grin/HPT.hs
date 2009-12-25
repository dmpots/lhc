module Grin.HPT
    ( analyze
    , lower
    , mkEnvironment
    , module Grin.HPT.Interface
    , Lhs (..)
    ) where

import Grin.Types               ( Grin )

import Grin.HPT.Environment     ( mkEnvironment, Lhs(..) )
--import Grin.HPT.Solve           ( )
import Grin.HPT.QuickSolve      ( )
import Grin.HPT.FastSolve       ( solve )
import Grin.HPT.Lower           ( lower )
import Grin.HPT.Interface

analyze :: Grin -> ([HeapAnalysis], HeapAnalysis)
analyze = solve . mkEnvironment

