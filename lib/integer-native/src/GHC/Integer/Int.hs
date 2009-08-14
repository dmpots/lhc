{-# LANGUAGE NoImplicitPrelude, CPP, MagicHash, UnboxedTuples #-}
module GHC.Integer.Int
   ( Int(..)
   , minInt
   , maxInt
   , plusInt
   , minusInt
   , timesInt
   , quotRemInt
   , quotInt
   , remInt
   , negateInt
   , absInt
   , signumInt
   , andInt
   , orInt
   , xorInt
   , complementInt
   , bitInt
   , shiftInt
   , shiftRInt
   , eqInt
   , neqInt
   , gtInt
   , geInt
   , ltInt
   , leInt
   , compareInt
   ) where

import GHC.Prim
import GHC.Types
import GHC.Ordering

eqInt (I# a) (I# b) = a ==# b

neqInt (I# a) (I# b) = a /=# b

gtInt (I# a) (I# b)
    = a ># b

geInt (I# a) (I# b)
    = a >=# b

ltInt (I# a) (I# b)
    = a <# b

leInt (I# a) (I# b)
    = a <=# b

signumInt (I# a)
    = if a ># 0#
      then I# 1#
      else if a ==# 0#
           then I# 0#
           else I# -1#

compareInt (I# a) (I# b)
    = if a ># b
      then GT
      else if a ==# b
           then EQ
           else LT

#if WORD_SIZE == 4
minInt  = I# (-0x80000000#)
maxInt  = I# 0x7FFFFFFF#
#else
minInt  = I# (-0x8000000000000000#)
maxInt  = I# 0x7FFFFFFFFFFFFFFF#
#endif

infixl 6 `plusInt`
plusInt :: Int -> Int -> Int
plusInt (I# a) (I# b) = I# (a +# b)

infixl 6 `minusInt`
minusInt (I# a) (I# b) = I# (a -# b)

infixl 7 `timesInt`
timesInt :: Int -> Int -> Int
timesInt (I# a) (I# b) = I# (a *# b)

quotRemInt :: Int -> Int -> (# Int, Int #)
quotRemInt (I# a) (I# b)
    = (# I# (a `quotInt#` b), I# (a `remInt#` b) #)

quotInt (I# a) (I# b)
    = I# (a `quotInt#` b)

remInt (I# a) (I# b)
    = I# (a `remInt#` b)

absInt (I# a)
    = if a >=# 0#
      then I# a
      else I# (negateInt# a)

negateInt (I# a) = I# (negateInt# a)

andInt :: Int -> Int -> Int
andInt (I# a) (I# b) = I# (word2Int# (int2Word# a `and#` int2Word# b))
orInt (I# a) (I# b) = I# (word2Int# (int2Word# a `or#` int2Word# b))
xorInt (I# a) (I# b) = I# (word2Int# (int2Word# a `xor#` int2Word# b))

complementInt (I# x#)
    = I# (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))

(I# x#) `shiftInt` (I# i#)
    = if  i# >=# 0#
      then I# (x# `iShiftL#` i#)
      else I# (x# `iShiftRA#` negateInt# i#)

a `shiftRInt` b = a `shiftInt` negateInt b

bitInt :: Int -> Int
bitInt n = I# 1# `shiftInt` n


#define WORD_SIZE_IN_BITS_ (WORD_SIZE# *# 8#)

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
iShiftL# :: Int# -> Int# -> Int#
a `iShiftL#` b
    = if b >=# WORD_SIZE_IN_BITS_
      then 0#
      else a `uncheckedIShiftL#` b                                                                                                                                                                                                                 
-- | Shift the argument right (signed) by the specified number of bits
-- (which must be non-negative).
iShiftRA# :: Int# -> Int# -> Int#
a `iShiftRA#` b
    = if b >=# WORD_SIZE_IN_BITS_
      then if a <# 0# then (-1#) else 0#
      else a `uncheckedIShiftRA#` b                                                                                                                                                                                                                 
-- | Shift the argument right (unsigned) by the specified number of bits
-- (which must be non-negative).
iShiftRL# :: Int# ->  Int# -> Int#
a `iShiftRL#` b
    = if b >=# WORD_SIZE_IN_BITS_
      then 0#
      else a `uncheckedIShiftRL#` b
    
