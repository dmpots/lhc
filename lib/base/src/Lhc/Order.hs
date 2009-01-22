{-# OPTIONS_LHC -N -fffi -fm4 #-}

module Lhc.Order(
    Bool(..),
    Ordering(..),
    Eq(..),
    Ord(..),
    (&&),
    (||),
    not,
    otherwise
    ) where

import Lhc.Types
import Lhc.Enum
import Lhc.Basics
import Lhc.Inst.Enum

data Bool = False | True
    deriving (Eq, Ord, Bounded, Enum)

data  Ordering    =  LT | EQ | GT
    deriving (Eq, Ord, Bounded, Enum)

infix  4  ==, /=, <, <=, >=, >

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = case x /= y of
        True -> False
        False -> True
    x /= y = case x == y of
        True -> False
        False -> True

class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y | x == y    = EQ
                | x <= y    = LT
                | otherwise = GT

    x <= y  = compare x y /= GT
    x <  y  = compare x y == LT
    x >= y  = compare x y /= LT
    x >  y  = compare x y == GT

    -- Note that (min x y, max x y) = (x,y) or (y,x)
    max x y | x <= y    =  y
            | otherwise =  x
    min x y | x <= y    =  x
            | otherwise =  y


instance Eq () where
    () == () = True
    () /= () = False

instance Ord () where
    () <= () = True
    () <  () = False
    () >= () = True
    () >  () = False
    max () () = ()
    min () () = ()
    compare () () = EQ

instance Bounded () where
    minBound = ()
    maxBound = ()

instance Eq a => Eq [a] where
    [] == [] = True
    (x:xs) == (y:ys) | x == y = xs == ys
    _ == _ = False

instance Ord a => Ord [a] where
    compare (x:xs) (y:ys) = case compare x y of
        EQ -> compare xs ys
        z -> z
    compare [] [] = EQ
    compare [] _ = LT
    compare _ [] = GT

    [] < [] = False
    [] < _ = True
    (x:xs) < (y:ys) = if x == y then xs < ys else x < y

    x > y = y < x

    x >= y = not (x < y)
    x <= y = not (y < x)

m4_define(ORDINST,{{
instance Eq $1 where
    $1 x == $1 y = boxBool (eq_$1 x y)
    $1 x /= $1 y = boxBool (neq_$1 x y)
instance Ord $1 where
    $1 x < $1 y = boxBool (lt_$1 x y)
    $1 x > $1 y = boxBool (gt_$1 x y)
    $1 x <= $1 y = boxBool (lte_$1 x y)
    $1 x >= $1 y = boxBool (gte_$1 x y)

foreign import primitive "Eq" eq_$1   :: $2 -> $2 -> Bool__
foreign import primitive "NEq" neq_$1 :: $2 -> $2 -> Bool__
foreign import primitive "$3Lt" lt_$1   :: $2 -> $2 -> Bool__
foreign import primitive "$3Lte" lte_$1 :: $2 -> $2 -> Bool__
foreign import primitive "$3Gt" gt_$1   :: $2 -> $2 -> Bool__
foreign import primitive "$3Gte" gte_$1 :: $2 -> $2 -> Bool__

}})

ORDINST(Word8,Bits8_,U)
ORDINST(Word16,Bits16_,U)
ORDINST(Word32,Bits32_,U)
ORDINST(Word64,Bits64_,U)
ORDINST(WordPtr,BitsPtr_,U)

ORDINST(Int8,Bits8_)
ORDINST(Int16,Bits16_)
ORDINST(Int32,Bits32_)
ORDINST(Int64,Bits64_)
ORDINST(IntPtr,BitsPtr_)

instance Eq Int where
    Int x == Int y = boxBool (bits32Eq x y)
    Int x /= Int y = boxBool (bits32NEq x y)

instance Ord Int where
    Int x < Int y = boxBool (bits32Lt x y)
    Int x > Int y = boxBool (bits32Gt x y)
    Int x <= Int y = boxBool (bits32Lte x y)
    Int x >= Int y = boxBool (bits32Gte x y)

instance Eq Word where
    Word x == Word y = boxBool (bits32Eq x y)
    Word x /= Word y = boxBool (bits32NEq x y)

instance Ord Word where
    Word x < Word y = boxBool (bits32Lt x y)
    Word x > Word y = boxBool (bits32Gt x y)
    Word x <= Word y = boxBool (bits32Lte x y)
    Word x >= Word y = boxBool (bits32Gte x y)

instance Eq Char where
    Char x == Char y = boxBool (bits32Eq x y)
    Char x /= Char y = boxBool (bits32NEq x y)

instance Ord Char where
    Char x < Char y = boxBool (bits32ULt x y)
    Char x > Char y = boxBool (bits32UGt x y)
    Char x <= Char y = boxBool (bits32ULte x y)
    Char x >= Char y = boxBool (bits32UGte x y)


instance Eq Integer where
    Integer x == Integer y = boxBool (bitsmaxEq x y)
    Integer x /= Integer y = boxBool (bitsmaxNEq x y)

instance Ord Integer where
    Integer x < Integer y = boxBool (bitsmaxLt x y)
    Integer x > Integer y = boxBool (bitsmaxGt x y)
    Integer x <= Integer y = boxBool (bitsmaxLte x y)
    Integer x >= Integer y = boxBool (bitsmaxGte x y)

instance Eq WordMax where
    WordMax x == WordMax y = boxBool (bitsmaxEq x y)
    WordMax x /= WordMax y = boxBool (bitsmaxNEq x y)

instance Ord WordMax where
    WordMax x < WordMax y = boxBool (bitsmaxLt x y)
    WordMax x > WordMax y = boxBool (bitsmaxGt x y)
    WordMax x <= WordMax y = boxBool (bitsmaxLte x y)
    WordMax x >= WordMax y = boxBool (bitsmaxGte x y)


infixr 3  &&
infixr 2  ||

{-# INLINE (&&), (||), not, otherwise #-}
(&&), (||)       :: Bool -> Bool -> Bool
True  && x       =  x
False && _       =  False
True  || _       =  True
False || x       =  x


not              :: Bool -> Bool
not x = if x then False else True


otherwise        :: Bool
otherwise        =  True


foreign import primitive "Eq" bits32Eq :: Int__ -> Int__ -> Bool__
foreign import primitive "NEq" bits32NEq :: Int__ -> Int__ -> Bool__
foreign import primitive "Lt" bits32Lt :: Int__ -> Int__ -> Bool__
foreign import primitive "Lte" bits32Lte :: Int__ -> Int__ -> Bool__
foreign import primitive "Gt" bits32Gt :: Int__ -> Int__ -> Bool__
foreign import primitive "Gte" bits32Gte :: Int__ -> Int__ -> Bool__
foreign import primitive "ULt" bits32ULt :: Char__ -> Char__ -> Bool__
foreign import primitive "ULte" bits32ULte :: Char__ -> Char__ -> Bool__
foreign import primitive "UGt" bits32UGt :: Char__ -> Char__ -> Bool__
foreign import primitive "UGte" bits32UGte :: Char__ -> Char__ -> Bool__

foreign import primitive "Eq" bitsmaxEq :: BitsMax_ -> BitsMax_ -> Bool__
foreign import primitive "NEq" bitsmaxNEq :: BitsMax_ -> BitsMax_ -> Bool__
foreign import primitive "Lt" bitsmaxLt :: BitsMax_ -> BitsMax_ -> Bool__
foreign import primitive "Lte" bitsmaxLte :: BitsMax_ -> BitsMax_ -> Bool__
foreign import primitive "Gt" bitsmaxGt :: BitsMax_ -> BitsMax_ -> Bool__
foreign import primitive "Gte" bitsmaxGte :: BitsMax_ -> BitsMax_ -> Bool__

foreign import primitive "box" boxBool :: Bool__ -> Bool

