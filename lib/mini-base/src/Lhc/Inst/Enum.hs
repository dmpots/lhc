{-# OPTIONS_LHC -N -fffi -funboxed-values -fm4 #-}

module Lhc.Inst.Enum() where

import Data.Word
import Data.Int
import Lhc.Enum
import Lhc.Num
import Lhc.Order
import Lhc.Types
import Lhc.IO(error)
import Lhc.Basics

m4_define(ENUMINST,{{
instance Enum $1 where
    toEnum = fromInt
    fromEnum = toInt
    succ = increment$1
    pred = decrement$1
    enumFrom c        = [c .. maxBound]
    enumFromThen c c' = last `seq` [c, c' .. last]
                      where last | c' < c    = minBound
                                 | otherwise = maxBound
    enumFromTo x y = f x where
        f x | x > y = []
            | otherwise = x:f (x + 1)
    enumFromThenTo x y z | y >= x = inc `seq` z `seq` f x where
        inc = y - x
        f x | x <= z = x:f (x + inc)
            | otherwise = []
    enumFromThenTo x y z  = dec `seq` z `seq` f x where
        dec = x - y
        f x | x >= z = x:f (x - dec)
            | otherwise = []

foreign import primitive "increment" increment$1 :: $1 -> $1
foreign import primitive "decrement" decrement$1 :: $1 -> $1

}})

ENUMINST(Word)
ENUMINST(Word8)
ENUMINST(Word16)
ENUMINST(Word32)
ENUMINST(Word64)
ENUMINST(WordMax)

ENUMINST(Int8)
ENUMINST(Int16)
ENUMINST(Int32)
ENUMINST(Int64)
{-ENUMINST(IntPtr)-}
ENUMINST(Integer)


instance Enum () where
    succ _      = error "Prelude.Enum.().succ: bad argument"
    pred _      = error "Prelude.Enum.().pred: bad argument"

    toEnum x | x == 0 = ()
             | otherwise    = error "Prelude.Enum.().toEnum: bad argument"

    fromEnum () = 0
    enumFrom () 	= [()]
    enumFromThen () () 	= let many = ():many in many
    enumFromTo () () 	= [()]
    enumFromThenTo () () () = let many = ():many in many

m4_define(INST_BOUNDED,{{
instance Bounded $1 where
    minBound = $1 (minBound$1 0#)
    maxBound = $1 (maxBound$1 0#)

foreign import primitive "minBound.$3" minBound$1 :: $2 -> $2
foreign import primitive "maxBound.$3" maxBound$1 :: $2 -> $2
}})

INST_BOUNDED(Int,Bits32_,bits32)
INST_BOUNDED(Int8,Bits8_,bits8)
INST_BOUNDED(Int16,Bits16_,bits16)
INST_BOUNDED(Int32,Bits32_,bits32)
INST_BOUNDED(Int64,Bits64_,bits64)

INST_BOUNDED(Word,Bits32_,bits32)
INST_BOUNDED(Word8,Bits8_,bits8)
INST_BOUNDED(Word16,Bits16_,bits16)
INST_BOUNDED(Word32,Bits32_,bits32)
INST_BOUNDED(Word64,Bits64_,bits64)
INST_BOUNDED(WordMax,BitsMax_,bits<max>)

-- Lemmih 2009.01.22: This instance shouldn't exist. Delete it.
INST_BOUNDED(Integer,BitsMax_,bits<max>)
