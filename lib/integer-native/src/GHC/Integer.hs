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
import GHC.IntWord64
import GHC.Integer.Internals

import qualified GHC.Integer.PureInteger as Pure

mkInteger i = i `seq` Integer i

toInt# :: Integer -> Int#
toInt# (Integer i) = Pure.intFromInteger i

eqInteger :: Integer -> Integer -> Bool
eqInteger (Integer a) (Integer b)
    = case Pure.compareInteger a b of
        GT -> False
        EQ -> True
        LT -> False

neqInteger :: Integer -> Integer -> Bool
neqInteger (Integer a) (Integer b)
    = case Pure.compareInteger a b of
        GT -> True
        EQ -> False
        LT -> True

ltInteger :: Integer -> Integer -> Bool
ltInteger (Integer a) (Integer b)
    = case Pure.compareInteger a b of
        GT -> False
        EQ -> False
        LT -> True

leInteger :: Integer -> Integer -> Bool
leInteger (Integer a) (Integer b)
    = case Pure.compareInteger a b of
        GT -> False
        EQ -> True
        LT -> True

gtInteger :: Integer -> Integer -> Bool
gtInteger (Integer a) (Integer b)
    = case Pure.compareInteger a b of
        GT -> True
        EQ -> False
        LT -> False

geInteger :: Integer -> Integer -> Bool
geInteger (Integer a) (Integer b)
    = case Pure.compareInteger a b of
        GT -> True
        EQ -> True
        LT -> False

compareInteger :: Integer -> Integer -> Ordering
compareInteger (Integer a) (Integer b)
    = Pure.compareInteger a b

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger (Integer a) (Integer b)
    = case Pure.quotRemInteger a b of
        (quot, rem) -> (# mkInteger quot, mkInteger rem #)

plusInteger :: Integer -> Integer -> Integer
plusInteger (Integer a) (Integer b) = mkInteger (Pure.addInteger a b)

minusInteger :: Integer -> Integer -> Integer
minusInteger (Integer a) (Integer b) = mkInteger (Pure.addInteger a (Pure.negateInteger b))

timesInteger :: Integer -> Integer -> Integer
timesInteger (Integer a) (Integer b) = mkInteger (Pure.multiplyInteger a b)

negateInteger :: Integer -> Integer
negateInteger (Integer a) = mkInteger (Pure.negateInteger a)

absInteger :: Integer -> Integer
absInteger (Integer n) = mkInteger (Pure.absInteger n)

signumInteger :: Integer -> Integer
signumInteger (Integer i)
    = mkInteger (Pure.signumInteger i)

smallInteger :: Int# -> Integer
smallInteger i
    = mkInteger (Pure.integerFromInt (I# i))

quotInteger :: Integer -> Integer -> Integer
quotInteger (Integer a) (Integer b)
    = mkInteger (a `Pure.quotInteger` b)

remInteger :: Integer -> Integer -> Integer
remInteger (Integer a) (Integer b)
    = mkInteger (a `Pure.remInteger` b)

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger a b
    = (# (a `divInteger` b), (a `modInteger` b) #)

lcmInteger :: Integer -> Integer -> Integer
lcmInteger a b = a

gcdInteger :: Integer -> Integer -> Integer
gcdInteger (Integer a) (Integer b) = mkInteger (a `gcdInt` b)

-- We can't throw an error here, so it is up to our caller to
-- not call us with both arguments being 0.
-- gcdInt 0# 0# = error "GHC.Integer.gcdInteger: gcd 0 0 is undefined"
gcdInt a b   = worker (Pure.absInteger a) (Pure.absInteger b)
    where worker a b = if Pure.isZeroInteger b then a else worker b (a `Pure.remInteger` b)

andInteger :: Integer -> Integer -> Integer
andInteger (Integer a) (Integer b)
    = mkInteger (Pure.tcAndInteger a b)

orInteger :: Integer -> Integer -> Integer
orInteger (Integer a) (Integer b)
    = mkInteger (Pure.tcOrInteger a b)

xorInteger :: Integer -> Integer -> Integer
xorInteger (Integer a) (Integer b)
    = mkInteger (Pure.tcXOrInteger a b)

complementInteger :: Integer -> Integer
complementInteger (Integer i)
    = mkInteger (Pure.tcComplementInteger i)

#if WORD_SIZE == 4
integerToWord64 :: Integer -> Word64#
integerToWord64 (Integer x) = int64ToWord64# (intToInt64# x)

integerToInt64 :: Integer -> Int64#
integerToInt64 (Integer x) = intToInt64# x

word64ToInteger :: Word64# -> Integer
word64ToInteger w = Integer (int64ToInt# (word64ToInt64# w))

int64ToInteger :: Int64# -> Integer
int64ToInteger i = smallInteger (int64ToInt# i)
#endif

wordToInteger :: Word# -> Integer
wordToInteger w = mkInteger (Pure.integerFromInt (I# (word2Int# w)))

integerToWord :: Integer -> Word#
integerToWord (Integer i) = int2Word# (Pure.intFromInteger i)

floatFromInteger :: Integer -> Float#
floatFromInteger (Integer i) = int2Float# 0#

doubleFromInteger :: Integer -> Double#
doubleFromInteger (Integer i) = int2Double# 0#

divInteger :: Integer -> Integer -> Integer
x `divInteger` y
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    = if (x `gtInteger` smallInteger 0#) && (y `ltInteger` smallInteger 0#)
      then ((x `minusInteger` smallInteger 1#) `quotInteger` y) `minusInteger` smallInteger 1#
      else if (x `ltInteger` smallInteger 0#) && (y `gtInteger` smallInteger 0#)
           then ((x `plusInteger` smallInteger 1#) `quotInteger` y) `minusInteger` smallInteger 1#
           else x `quotInteger` y

modInteger :: Integer -> Integer -> Integer
x `modInteger` y
    = if (x `gtInteger` smallInteger 0#) && (y `ltInteger` smallInteger 0#) ||
         (x `ltInteger` smallInteger 0#) && (y `gtInteger` smallInteger 0#)
      then if r `neqInteger` smallInteger 0# then r `plusInteger` y else smallInteger 0#
      else r
    where
    r = x `remInteger` y

True && True = True
_    && _    = False
otherwise = True
False || False = False
_     || _     = True

