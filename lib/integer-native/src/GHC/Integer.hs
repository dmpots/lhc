module GHC.Integer
    ( Integer
    , toInt#
    , eqInteger
    , neqInteger
    , ltInteger
    , leInteger
    , gtInteger
    , geInteger
    , compareInteger
    , quotRemInteger
    , plusInteger
    , minusInteger
    , timesInteger
    , negateInteger
    , absInteger
    , signumInteger
    , smallInteger
    , quotInteger
    , remInteger
    , divModInteger
    , lcmInteger
    , gcdInteger
    , andInteger
    , orInteger
    , xorInteger
    , complementInteger
#if WORD_SIZE == 4
    , integerToWord64
    , integerToInt64
    , word64ToInteger
    , int64ToInteger
#endif
    , wordToInteger
    , integerToWord
    , floatFromInteger
    , doubleFromInteger
    ) where

import GHC.Types
import GHC.Prim
import GHC.Bool
import GHC.Ordering

import GHC.Integer.Internals

toInt# :: Integer -> Int#
toInt# (Integer i) = i

eqInteger :: Integer -> Integer -> Bool
eqInteger (Integer a) (Integer b) = a ==# b

neqInteger :: Integer -> Integer -> Bool
neqInteger (Integer a) (Integer b) = a /=# b

ltInteger :: Integer -> Integer -> Bool
ltInteger (Integer a) (Integer b) = a <# b

leInteger :: Integer -> Integer -> Bool
leInteger (Integer a) (Integer b) = a <=# b

gtInteger :: Integer -> Integer -> Bool
gtInteger (Integer a) (Integer b) = a ># b

geInteger :: Integer -> Integer -> Bool
geInteger (Integer a) (Integer b) = a >=# b

compareInteger :: Integer -> Integer -> Ordering
compareInteger (Integer a) (Integer b)
    = if a ># b
      then GT
      else if a ==# b
           then EQ
           else LT

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger (Integer a) (Integer b)
    = (# Integer (a `quotInt#` b), Integer (a `remInt#` b) #)

plusInteger :: Integer -> Integer -> Integer
plusInteger (Integer a) (Integer b) = Integer (a +# b)

minusInteger :: Integer -> Integer -> Integer
minusInteger (Integer a) (Integer b) = Integer (a -# b)

timesInteger :: Integer -> Integer -> Integer
timesInteger (Integer a) (Integer b) = Integer (a *# b)

negateInteger :: Integer -> Integer
negateInteger (Integer a) = Integer (negateInt# a)

absInteger :: Integer -> Integer
absInteger (Integer n) = Integer (if n ># 0# then n else negateInt# n)

signumInteger :: Integer -> Integer
signumInteger (Integer i)
    = if i <# 0#
      then Integer (negateInt# 1#) else if i ==# 0#
      then Integer 0#
      else Integer 1#

smallInteger :: Int# -> Integer
smallInteger i
    = Integer i

quotInteger :: Integer -> Integer -> Integer
quotInteger (Integer a) (Integer b)
    = Integer (a `quotInt#` b)

remInteger :: Integer -> Integer -> Integer
remInteger (Integer a) (Integer b)
    = Integer (a `remInt#` b)

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger (Integer a) (Integer b)
    = (# Integer (a `divInt#` b), Integer (a `modInt#` b) #)

lcmInteger :: Integer -> Integer -> Integer
lcmInteger a b = a

gcdInteger :: Integer -> Integer -> Integer
gcdInteger (Integer a) (Integer b) = Integer (a `gcdInt#` b)

andInteger :: Integer -> Integer -> Integer
andInteger (Integer a) (Integer b)
    = Integer (word2Int# (int2Word# a `and#` int2Word# b))

orInteger :: Integer -> Integer -> Integer
orInteger (Integer a) (Integer b)
    = Integer (word2Int# (int2Word# a `or#` int2Word# b))

xorInteger :: Integer -> Integer -> Integer
xorInteger (Integer a) (Integer b)
    = Integer (word2Int# (int2Word# a `xor#` int2Word# b))

complementInteger :: Integer -> Integer
complementInteger (Integer x#)
    = Integer (word2Int# (int2Word# x# `xor#` int2Word# (-1#)))

#if WORD_SIZE == 4
integerToWord64 :: Integer -> Word64#
integerToWord64 (Integer x#) = int64ToWord64# (intToInt64# i)

integerToInt64 :: Integer -> Int64#
integerToInt64 (Integer x#) = intToInt64# i

word64ToInteger :: Word64# -> Integer
word64ToInteger w = Integer (int64ToInt# (word64ToInt64# w))

int64ToInteger :: Int64# -> Integer
int64ToInteger i = smallInteger (int64ToInt# i)
#endif

wordToInteger :: Word# -> Integer
wordToInteger w = Integer (word2Int# w)

integerToWord :: Integer -> Word#
integerToWord (Integer i) = int2Word# i

floatFromInteger :: Integer -> Float#
floatFromInteger (Integer i) = int2Float# i

doubleFromInteger :: Integer -> Double#
doubleFromInteger (Integer i) = int2Double# i

divInt# :: Int# -> Int# -> Int#
x# `divInt#` y#
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    = if  (x# ># 0#) && (y# <# 0#)
      then ((x# -# 1#) `quotInt#` y#) -# 1#
      else if (x# <# 0#) && (y# ># 0#) 
           then ((x# +# 1#) `quotInt#` y#) -# 1#
           else x# `quotInt#` y#

modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
    = if (x# ># 0#) && (y# <# 0#) ||
         (x# <# 0#) && (y# ># 0#)
      then if r# /=# 0# then r# +# y# else 0#
      else r#
    where
    r# = x# `remInt#` y#

True && True = True
_    && _    = False
otherwise = True
False || False = False
_     || _     = True

