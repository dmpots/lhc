{-# OPTIONS_LHC -N -fffi -funboxed-values -fm4 #-}

module Lhc.Inst.Enum() where

import Data.Word
import Data.Int
import Lhc.Enum
import Lhc.Num
import Lhc.Order
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


instance Bounded Int where
    minBound = Int 0x80000000#
    maxBound = Int 0x7FFFFFFF#
instance Bounded Int8 where
    minBound = Int8 0x80#
    maxBound = Int8 0x7F#
instance Bounded Int16 where
    minBound = Int16 0x8000#
    maxBound = Int16 0x7FFF#
instance Bounded Int32 where
    minBound = Int32 0x80000000#
    maxBound = Int32 0x7FFFFFFF#
instance Bounded Int64 where
    minBound = Int64 0x8000000000000000#
    maxBound = Int64 0x7FFFFFFFFFFFFFFF#

instance Bounded Word where
    minBound = Word 0#
    maxBound = Word 0xFFFFFFFF#
instance Bounded Word8 where
    minBound = Word8 0#
    maxBound = Word8 0xFF#
instance Bounded Word16 where
    minBound = Word16 0#
    maxBound = Word16 0xFFFF#
instance Bounded Word32 where
    minBound = Word32 0#
    maxBound = Word32 0xFFFFFFFF#
instance Bounded Word64 where
    minBound = Word64 0#
    maxBound = Word64 0xFFFFFFFFFFFFFFFF#

instance Bounded WordMax where
    minBound = bitsMaxMinBound
    maxBound = bitsMaxMaxBound

-- Lemmih 2009.01.22: These primitives don't exist yet.
foreign import primitive "MinBound"      bitsMaxMinBound :: WordMax
foreign import primitive "MaxBound"      bitsMaxMaxBound :: WordMax

-- Lemmih 2009.01.22: This instance shouldn't exist. Delete it.
instance Bounded Integer where
    minBound = Integer 0#
    maxBound = Integer 0xFFFFFFFF#

