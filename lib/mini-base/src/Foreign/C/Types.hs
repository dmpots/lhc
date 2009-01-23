{-# OPTIONS --noprelude -fffi -fm4 -funboxed-tuples -funboxed-values #-}
module Foreign.C.Types
  ( CChar(..)
  , CInt(..)
  , CWchar(..)
  , CWint
  , CSize
  ) where

-- FIXME: this module really shouldn't import all this stuff. These types should probably
-- be defined in another module so that instances can be defined elsewhere.

import Lhc.Types
import Lhc.Num
import Lhc.Order
import Lhc.Prim
import Lhc.Basics
import Foreign.Storable
import Lhc.IO
import Lhc.Int
import Lhc.Addr

data CChar = CChar Bits8_
data CSChar
data CUChar
data CShort
data CUShort
data CInt = CInt BitsInt_
data CUInt
data CLong
data CULong
data CPtrdiff
data CSize = CSize BitsSize_t_
data CWchar = CWchar Bits32_
data CSigAtomic
data CLLong
data CULLong
data CClock
data CTime
data CFloat
data CDouble
data CLDouble
data CFile
data CJmpBuf
data CFpos
data CWint

m4_include(Lhc/Inst/Num.m4)
m4_include(Lhc/Order.m4)
m4_include(Foreign/Storable.m4)

INST_EQORDER(CInt,BitsInt_)
NUMINST(CInt, BitsInt_)
INST_STORABLE(CInt,BitsInt_,bits<int>)

INST_EQORDER(CSize,BitsSize_t_)
NUMINST(CSize, BitsSize_t_)
INST_STORABLE(CSize,BitsSize_t_,bits<size_t>)

INST_EQORDER(CWchar,Bits32_)
NUMINST(CWchar, Bits32_)
INST_STORABLE(CWchar,Bits32_,bits32)

INST_EQORDER(CChar,Bits8_)
NUMINST(CChar, Bits8_)
INST_STORABLE(CChar,Bits8_,bits8)

foreign import primitive "box" boxBool :: Bool__ -> Bool
