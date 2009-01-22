{-# OPTIONS_LHC -N -fffi -fm4 #-}
module Lhc.Num where

import Lhc.Types
import Lhc.Basics
import Lhc.Order
import Lhc.Show
import Lhc.IO(error)
import Lhc.Enum
import Lhc.Float

import Data.Word

infixl 7 :%
infixl 7  *  , /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

data  Ratio a  = !a :% !a
type  Rational = Ratio Integer

numerator, denominator  :: Ratio a -> a
numerator (x :% _)      =  x
denominator (_ :% y)    =  y


class  (Eq a, Show a) => Num a  where
    (+), (-), (*)    :: a -> a -> a
    negate           :: a -> a
    abs, signum      :: a -> a
    fromInteger      :: Integer -> a
    fromInt          :: Int -> a

        -- Minimal complete definition:
        --      All, except negate or (-)
    x - y            =  x + negate y
    negate x         =  0 - x
    fromInt i = fromInteger (toInteger i)
    fromInteger x = fromInt (toInt x)

class  (Num a, Ord a) => Real a  where
    toRational       ::  a -> Rational
    toDouble         ::  a -> Double
    toDouble x = rationalToDouble (toRational x)

class  (Real a, Enum a) => Integral a  where
    quot, rem        :: a -> a -> a
    div, mod         :: a -> a -> a
    quotRem, divMod  :: a -> a -> (a,a)
    toInteger        :: a -> Integer
    toInt            :: a -> Int

        -- Minimal complete definition:
        --      quotRem, toInteger
    n `quot` d       =  q  where (q,r) = quotRem n d
    n `rem` d        =  r  where (q,r) = quotRem n d
    n `div` d        =  q  where (q,r) = divMod n d
    n `mod` d        =  r  where (q,r) = divMod n d
    divMod n d       =  if signum r == - signum d then (q-1, r+d) else qr
                        where qr@(q,r) = quotRem n d
    quotRem n d       =  (n `quot` d, n `rem` d)
    toInteger x = toInteger (toInt x)
    toInt x = toInt (toInteger x)

class  (Num a) => Fractional a  where
    (/)              :: a -> a -> a
    recip            :: a -> a
    fromRational     :: Rational -> a
    fromDouble       :: Double   -> a

        -- Minimal complete definition:
        --      fromRational and (recip or (/))
    recip x          =  1 / x
    x / y            =  x * recip y

    --fromDouble x = fromRational (doubleToRational x)


fromIntegral   :: (Integral a, Num b) => a -> b
fromIntegral x =  fromInteger (toInteger x)

realToFrac     :: (Real a, Fractional b) => a -> b
realToFrac x   =  fromRational (toRational x)

{-# RULES
  "realToFrac/toRational"     realToFrac = toRational
  "realToFrac/fromRational"   realToFrac = fromRational
  "realToFrac/toDouble"       realToFrac = toDouble
  "realToFrac/fromDouble"     realToFrac = fromDouble
 #-}

{-# RULES
  "fromIntegral/Int"          fromIntegral = (id :: Int -> Int)
  "fromIntegral/Integer"      fromIntegral = (id :: Integer -> Integer)
  "fromIntegral/toInt"        fromIntegral = toInt
  "fromIntegral/fromInt"      fromIntegral = fromInt
  "fromIntegral/toInteger"    fromIntegral = toInteger
  "fromIntegral/fromInteger"  fromIntegral = fromInteger
 #-}


{-# INLINE subtract #-}
subtract         :: (Num a) => a -> a -> a
subtract         =  flip (-)

{-# INLINE even #-}
{-# INLINE odd #-}

even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even



m4_define(NUMINST,{{
instance Num $1 where
    $1 x + $1 y = $1 (add_$1 x y)
    $1 x - $1 y = $1 (sub_$1 x y)
    $1 x * $1 y = $1 (mul_$1 x y)
    
    negate ($1 x) = $1 (neg_$1 x)
    
    abs    x | x < 0     = -x
             | otherwise =  x
    
    signum 0 = 0
    signum x | x < 0     = -1
             | otherwise =  1

    fromInteger (Integer x) = $1 (bitsmax_to_$1 x)
    fromInt (Int x) = $1 (bits32_to_$1 x)

instance Integral $1 where
    $1 n `quot` $1 d = $1 (div_$1 n d)
    $1 n `rem`  $1 d = $1 (mod_$1 n d)

    toInteger ($1 x) = Integer (max_from_$1 x)
    toInt ($1 x) = Int (bits32_from_$1 x)

foreign import primitive "Neg" neg_$1 :: $2 -> $2
foreign import primitive "Add" add_$1 :: $2 -> $2 -> $2
foreign import primitive "Sub" sub_$1 :: $2 -> $2 -> $2
foreign import primitive "Mul" mul_$1 :: $2 -> $2 -> $2
foreign import primitive "Div" div_$1 :: $2 -> $2 -> $2
foreign import primitive "Mod" mod_$1 :: $2 -> $2 -> $2

foreign import primitive "I2I" bitsmax_to_$1 :: BitsMax_ -> $2
foreign import primitive "I2I" bits32_to_$1 :: Bits32_ -> $2
foreign import primitive "I2I" max_from_$1 :: $2 -> BitsMax_
foreign import primitive "I2I" bits32_from_$1 :: $2 -> Bits32_
}})



NUMINST(Int,Bits32_)
NUMINST(Int8,Bits8_)
NUMINST(Int16,Bits16_)
NUMINST(Int32,Bits32_)
NUMINST(Int64,Bits64_)
NUMINST(IntPtr,BitsPtr_)
NUMINST(Integer,BitsMax_)

NUMINST(Word,Bits32_)
NUMINST(Word8,Bits8_)
NUMINST(WordMax,BitsMax_)
NUMINST(Word16,Bits16_)
NUMINST(Word32,Bits32_)
NUMINST(Word64,Bits64_)
NUMINST(WordPtr,BitsPtr_)

