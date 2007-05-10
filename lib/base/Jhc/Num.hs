{-# OPTIONS_JHC -N #-}
module Jhc.Num where

import Jhc.Basics
import Jhc.Order
import Jhc.Show
import Jhc.IO(error)
import Jhc.Enum

infixl 7  *  , /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

data  Ratio a  = !a :% !a
type  Rational = Ratio Integer


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

        -- Minimal complete definition:
        --      fromRational and (recip or (/))
    recip x          =  1 / x
    x / y            =  x * recip y


fromIntegral     :: (Integral a, Num b) => a -> b
fromIntegral     =  fromInteger . toInteger

realToFrac     :: (Real a, Fractional b) => a -> b
realToFrac      =  fromRational . toRational

{-# RULES
  "realToFrac/toRational"     realToFrac = toRational
  "realToFrac/fromRational"   realToFrac = fromRational
 #-}

{-# RULES
  "fromIntegral/Int"          fromIntegral = (id :: Int -> Int)
  "fromIntegral/Integer"      fromIntegral = (id :: Integer -> Integer)
  "fromIntegral/toInt"        fromIntegral = toInt
  "fromIntegral/fromInt"      fromIntegral = fromInt
  "fromIntegral/toInteger"    fromIntegral = toInteger
  "fromIntegral/fromInteger"  fromIntegral = fromInteger
 #-}