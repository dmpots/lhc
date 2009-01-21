{-# OPTIONS_LHC -N -fffi #-}
module Lhc.Num where

import Lhc.Types
import Lhc.Basics
import Lhc.Order
import Lhc.Show
import Lhc.IO(error)
import Lhc.Enum
import Lhc.Float

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


instance Num Int where
    Int x + Int y = Int (bits32_Add x y)
    Int x - Int y = Int (bits32_Sub x y)
    Int x * Int y = Int (bits32_Mul x y)
    
    negate (Int x) = Int (bits32_Neg x)
    
    abs    x | x < 0     = -x
             | otherwise =  x
    
    signum 0 = 0
    signum x | x < 0     = -1
             | otherwise =  1

    fromInteger (Integer x) = Int (bitsmax_to_32 x)
    fromInt = id

instance Integral Int where
    Int n `quot` Int d = Int (bits32_Div n d)
    Int n `rem`  Int d = Int (bits32_Mod n d)

    toInteger (Int x) = Integer (bits32_to_max x)
    toInt = id

foreign import primitive "Neg" bits32_Neg :: Bits32_ -> Bits32_
foreign import primitive "Add" bits32_Add :: Bits32_ -> Bits32_ -> Bits32_
foreign import primitive "Sub" bits32_Sub :: Bits32_ -> Bits32_ -> Bits32_
foreign import primitive "Mul" bits32_Mul :: Bits32_ -> Bits32_ -> Bits32_
foreign import primitive "Div" bits32_Div :: Bits32_ -> Bits32_ -> Bits32_
foreign import primitive "Mod" bits32_Mod :: Bits32_ -> Bits32_ -> Bits32_

foreign import primitive "I2I" bitsmax_to_32 :: BitsMax_ -> Bits32_
foreign import primitive "I2I" bits32_to_max :: Bits32_ -> BitsMax_


instance Num Integer where
    Integer x + Integer y = Integer (bitsmax_Add x y)
    Integer x - Integer y = Integer (bitsmax_Sub x y)
    Integer x * Integer y = Integer (bitsmax_Mul x y)
    
    negate (Integer x) = Integer (bitsmax_Neg x)
    
    abs    x | x < 0     = -x
             | otherwise =  x
    
    signum 0 = 0
    signum x | x < 0     = -1
             | otherwise =  1

    fromInt (Int x) = Integer (bits32_to_max x)
    fromInteger = id

instance Integral Integer where
    Integer n `quot` Integer d = Integer (bitsmax_Div n d)
    Integer n `rem`  Integer d = Integer (bitsmax_Mod n d)

    toInt (Integer x) = Int (bitsmax_to_32 x)
    toInteger = id

foreign import primitive "Neg" bitsmax_Neg :: BitsMax_ -> BitsMax_
foreign import primitive "Add" bitsmax_Add :: BitsMax_ -> BitsMax_ -> BitsMax_
foreign import primitive "Sub" bitsmax_Sub :: BitsMax_ -> BitsMax_ -> BitsMax_
foreign import primitive "Mul" bitsmax_Mul :: BitsMax_ -> BitsMax_ -> BitsMax_
foreign import primitive "Div" bitsmax_Div :: BitsMax_ -> BitsMax_ -> BitsMax_
foreign import primitive "Mod" bitsmax_Mod :: BitsMax_ -> BitsMax_ -> BitsMax_
