module Grin.HPT
    ( analyze
    , lower
    ) where

import Grin.Types               ( Grin )

import Grin.HPT.Environment     ( mkEnvironment )
import Grin.HPT.Solve           ( solve, HeapAnalysis )
import Grin.HPT.Lower           ( lower )


analyze :: Grin -> (Int, HeapAnalysis)
analyze = solve . mkEnvironment

