{-# OPTIONS_LHC -N -fffi -funboxed-values #-}
module Lhc.Enum(Enum(..),Bounded(..)) where
-- Enumeration and Bounded classes

import Lhc.Inst.PrimEnum()
import Data.Int
import Lhc.Types
import Lhc.Basics
import Lhc.Order
import Lhc.Int

class  Enum a  where
    succ, pred       :: a -> a
    toEnum           :: Int -> a
    fromEnum         :: a -> Int
    enumFrom         :: a -> [a]             -- [n..]
    enumFromThen     :: a -> a -> [a]        -- [n,n'..]
    enumFromTo       :: a -> a -> [a]        -- [n..m]
    enumFromThenTo   :: a -> a -> a -> [a]   -- [n,n'..m]

        -- Minimal complete definition:
        --      toEnum, fromEnum
--
-- NOTE: these default methods only make sense for types
--   that map injectively into Int using fromEnum
--  and toEnum.
    succ             =  toEnum . increment . fromEnum
    pred             =  toEnum . decrement . fromEnum
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThen x y =  map toEnum [fromEnum x, fromEnum y ..]
    enumFromThenTo x y z =
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]


class Bounded a  where
    minBound         :: a
    maxBound         :: a

instance Enum Int where
    succ = increment
    pred = decrement
    toEnum x = x
    fromEnum x = x

    enumFrom x  | x `seq` True  =  enumFromTo x maxBound
    enumFromThen c c' = [c, c' .. lastInt]
                      where lastInt | c' < c    = minBound
                                    | otherwise = maxBound
    enumFromTo x y = f x where
        f x | x > y = []
            | otherwise = x:f (increment x)

    enumFromThenTo = efdtInt


----------------------------------------------------------------------
-- Shamelessly stolen from GHC.Enum

efdtInt :: Int -> Int -> Int -> [Int]
-- [x1,x2..y]
efdtInt x1 x2 y
 | x2 >= x1  = efdtIntUp x1 x2 y
 | otherwise = efdtIntDn x1 x2 y

-- Requires x2 >= x1
efdtIntUp :: Int -> Int -> Int -> [Int]
efdtIntUp x1 x2 y    -- Be careful about overflow!
 | y < x2    = if y < x1 then [] else [x1]
 | otherwise = -- Common case: x1 <= x2 <= y
               let delta = x2 `minus` x1 -- >= 0
                   y' = y `minus` delta  -- x1 <= y' <= y; hence y' is representable

                   -- Invariant: x <= y
                   -- Note that: z <= y' => z + delta won't overflow
                   -- so we are guaranteed not to overflow if/when we recurse
                   go_up x | x > y'   = [x]
                           | otherwise = x : go_up (x `plus` delta)
               in x1 : go_up x2

-- Requires x2 <= x1
efdtIntDn :: Int -> Int -> Int -> [Int]
efdtIntDn x1 x2 y    -- Be careful about underflow!
 | y > x2    = if y > x1 then [] else [x1]
 | otherwise = -- Common case: x1 >= x2 >= y
               let delta = x2 `minus` x1 -- <= 0
                   y' = y `minus` delta  -- y <= y' <= x1; hence y' is representable

                   -- Invariant: x >= y
                   -- Note that: z >= y' => z + delta won't underflow
                   -- so we are guaranteed not to underflow if/when w-e recurse
                   go_dn x | x < y'   = [x]
                           | otherwise = x : go_dn (x `plus` delta)
               in x1 : go_dn x2

-- End shameless theft
----------------------------------------------------------------------


instance Enum Char where
    toEnum = chr
    fromEnum = ord
    enumFrom c        = [c .. maxBound::Char]
    enumFromThen c c' = [c, c' .. lastChar]
                      where lastChar :: Char
                            lastChar | c' < c    = minBound
                                     | otherwise = maxBound
--    enumFromTo (Char x) (Char y) = f x where
--        f x = case x `bits32UGt` y of
--            0# -> []
--            1# -> Char x:f (bits32Increment x)
--    enumFromThenTo (Char x) (Char y) (Char z) =
--        case y `bits32Sub` x of
--            inc -> let f x = case x `bits32UGte` z of
--                            1# -> Char x:f (x `bits32Add` inc)
--                            0# -> []
--             in f x


instance Bounded Char where
    minBound = Char 0#
    maxBound = Char 0x10ffff#

foreign import primitive "UGt"       bits32UGt       :: Bits32_ -> Bits32_ -> Bool__
foreign import primitive "UGte"      bits32UGte      :: Bits32_ -> Bits32_ -> Bool__
foreign import primitive "increment" bits32Increment :: Bits32_ -> Bits32_

foreign import primitive "Add"       bits32Add       :: Bits32_ -> Bits32_ -> Bits32_
foreign import primitive "Sub"       bits32Sub       :: Bits32_ -> Bits32_ -> Bits32_



