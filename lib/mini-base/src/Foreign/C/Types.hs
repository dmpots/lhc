{-# OPTIONS --noprelude -fm4 #-}
module Foreign.C.Types where

import Data.Int
import Data.Word
import Lhc.Num
import Lhc.Enum
import Lhc.Addr
import Foreign.Storable
import Lhc.Inst.Storable
import Lhc.Monad
import Lhc.Order

data CChar = CChar Int8 deriving (Bounded)
data CSChar
data CUChar
data CShort
data CUShort
data CInt
data CUInt
data CLong
data CULong
data CPtrdiff
data CSize = CSize Word64 deriving (Bounded)
data CWchar = CWchar Int32 deriving (Bounded)
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


m4_define(INSTS,{{
instance Num $1 where
    $1 x + $1 y = $1 (x + y)
    $1 x - $1 y = $1 (x - y)
    $1 x * $1 y = $1 (x * y)

    negate ($1 x) = $1 (negate x)
    abs    ($1 x) = $1 (abs x)
    signum ($1 x) = $1 (signum x)
    fromInteger x = $1 (fromInteger x)
    fromInt x = $1 (fromInt x)

instance Integral $1 where
    $1 n `quot` $1 d = $1 (quot n d)
    $1 n `rem`  $1 d = $1 (rem n d)

    toInteger ($1 x) = toInteger x
    toInt ($1 x) = toInt x
{-
instance Bounded $1 where
    minBound = $1 minBound
    maxBound = $1 maxBound
-}

instance Storable $1 where
    peek ptr = do x <- peek (castPtr ptr)
                  return ($1 x)
    poke ptr ($1 x) = poke (castPtr ptr) x
    sizeOf ~($1 x) = sizeOf x
    alignment ~($1 x) = alignment x

instance Eq $1 where
    $1 x == $1 y = x == y
    $1 x /= $1 y = x /= y

instance Ord $1 where
    $1 x `compare` $1 y = x `compare` y

}})

INSTS(CSize)
INSTS(CWchar)
INSTS(CChar)
