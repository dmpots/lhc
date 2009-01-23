{-# OPTIONS --noprelude -fffi -fm4 #-}
module Foreign.C.Types
  ( CChar(..)
  , CInt(..)
  ) where

import Lhc.Types
import Lhc.Num
import Lhc.Order
import Lhc.Prim
import Lhc.Basics

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
data CSize
data CWchar
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

INST_EQORDER(CInt,BitsInt_)
NUMINST(CInt, BitsInt_)

foreign import primitive "box" boxBool :: Bool__ -> Bool
