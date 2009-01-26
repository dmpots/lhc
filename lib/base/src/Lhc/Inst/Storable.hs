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


INST_STORABLE(Int8,Bits8_,bits8)
INST_STORABLE(Int32,Bits32_,bits32)
INST_STORABLE(Word64,Bits64_,bits64)
INST_STORABLE(Float,Float32_,fbits32)
INST_STORABLE(Double,Float64_,fbits64)


