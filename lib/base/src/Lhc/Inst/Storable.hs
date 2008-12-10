{-# OPTIONS_LHC -fm4 -N -fffi -funboxed-tuples -funboxed-values #-}
module Lhc.Inst.Storable where

m4_include(Foreign/Storable.m4)

import Lhc.Types
import Lhc.Float
import Foreign.Storable
import Lhc.Prim
import Lhc.Basics
import Lhc.Addr
import Lhc.Int
import Lhc.IO


INST_STORABLE(Float,Float32_,fbits32)
INST_STORABLE(Double,Float64_,fbits64)


