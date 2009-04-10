{-# OPTIONS_LHC -N -fffi -fm4 #-}
module Data.Bits where


import Lhc.Num
import Lhc.Order
import Lhc.Types
import Data.Int
import Lhc.Prim
import Lhc.Basics

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.


{-|
The 'Bits' class defines bitwise operations over integral types.

* Bits are numbered from 0 with bit 0 being the least
  significant bit.
-}
class Num a => Bits a where
    -- | Bitwise \"and\"
    (.&.) :: a -> a -> a

    -- | Bitwise \"or\"
    (.|.) :: a -> a -> a

    -- | Bitwise \"xor\"
    xor :: a -> a -> a

    {-| Reverse all the bits in the argument -}
    complement        :: a -> a

    {-| Shift the argument left by the specified number of bits.
	Right shifts (signed) are specified by giving a negative value.

	An instance can define either this unified 'shift' or 'shiftL' and
	'shiftR', depending on which is more convenient for the type in
	question. -}
    shift             :: a -> Int -> a

    x `shift`   i | i<0  = x `shiftR` (-i)
                  | i==0 = x
                  | i>0  = x `shiftL` i

    {-| Rotate the argument left by the specified number of bits.
	Right rotates are specified by giving a negative value.

        For unbounded types like 'Integer', 'rotate' is equivalent to 'shift'.

	An instance can define either this unified 'rotate' or 'rotateL' and
	'rotateR', depending on which is more convenient for the type in
	question. -}
    rotate            :: a -> Int -> a

    x `rotate`  i | i<0  = x `rotateR` (-i)
                  | i==0 = x
                  | i>0  = x `rotateL` i

    {-
    -- Rotation can be implemented in terms of two shifts, but care is
    -- needed for negative values.  This suggested implementation assumes
    -- 2's-complement arithmetic.  It is commented out because it would
    -- require an extra context (Ord a) on the signature of 'rotate'.
    x `rotate`  i | i<0 && isSigned x && x<0
                         = let left = i+bitSize x in
                           ((x `shift` i) .&. complement ((-1) `shift` left))
                           .|. (x `shift` left)
                  | i<0  = (x `shift` i) .|. (x `shift` (i+bitSize x))
                  | i==0 = x
                  | i>0  = (x `shift` i) .|. (x `shift` (i-bitSize x))
    -}

    -- | @bit i@ is a value with the @i@th bit set
    bit               :: Int -> a

    -- | @x \`setBit\` i@ is the same as @x .|. bit i@
    setBit            :: a -> Int -> a

    -- | @x \`clearBit\` i@ is the same as @x .&. complement (bit i)@
    clearBit          :: a -> Int -> a

    -- | @x \`complementBit\` i@ is the same as @x \`xor\` bit i@
    complementBit     :: a -> Int -> a

    -- | Return 'True' if the @n@th bit of the argument is 1
    testBit           :: a -> Int -> Bool

    {-| Return the number of bits in the type of the argument.  The actual
	value of the argument is ignored.  The function 'bitSize' is
	undefined for types that do not have a fixed bitsize, like 'Integer'.
	-}
    bitSize           :: a -> Int

    {-| Return 'True' if the argument is a signed type.  The actual
        value of the argument is ignored -}
    isSigned          :: a -> Bool

    bit i               = 1 `shiftL` i
    x `setBit` i        = x .|. bit i
    x `clearBit` i      = x .&. complement (bit i)
    x `complementBit` i = x `xor` bit i
    x `testBit` i       = (x .&. bit i) /= 0

    {-| Shift the argument left by the specified number of bits
	(which must be non-negative).

	An instance can define either this and 'shiftR' or the unified
	'shift', depending on which is more convenient for the type in
	question. -}
    shiftL            :: a -> Int -> a
    x `shiftL`  i = x `shift`  i

    {-| Shift the argument right (signed) by the specified number of bits
	(which must be non-negative).

	An instance can define either this and 'shiftL' or the unified
	'shift', depending on which is more convenient for the type in
	question. -}
    shiftR            :: a -> Int -> a
    x `shiftR`  i = x `shift`  (-i)

    {-| Rotate the argument left by the specified number of bits
	(which must be non-negative).

	An instance can define either this and 'rotateR' or the unified
	'rotate', depending on which is more convenient for the type in
	question. -}
    rotateL           :: a -> Int -> a
    x `rotateL` i = x `rotate` i

    {-| Rotate the argument right by the specified number of bits
	(which must be non-negative).

	An instance can define either this and 'rotateL' or the unified
	'rotate', depending on which is more convenient for the type in
	question. -}
    rotateR           :: a -> Int -> a
    x `rotateR` i = x `rotate` (-i)


m4_define(INST_BITS,{{
instance Bits $1 where
    $1 x .&. $1 y = $1 (and$1 x y)
    $1 x .|. $1 y = $1 (or$1 x y)
    $1 x `xor` $1 y = $1 (xor$1 x y)
    complement ($1 x) = $1 (complement$1 x)
    shiftL ($1 x) (Int bits) = $1 (shiftL$1 x bits)
    shiftR ($1 x) (Int bits) = $1 (shiftR$1 x bits)

foreign import primitive "And" and$1 :: $2 -> $2 -> $2
foreign import primitive "Or" or$1 :: $2 -> $2 -> $2
foreign import primitive "Xor" xor$1 :: $2 -> $2 -> $2
foreign import primitive "Com" complement$1 :: $2 -> $2
foreign import primitive "Shl" shiftL$1 :: $2 -> Bits32_ -> $2
m4_ifelse(`$3',`S',foreign import primitive "Shra" shiftR$1 :: $2 -> Bits32_ -> $2,
                   foreign import primitive "Shr" shiftR$1 :: $2 -> Bits32_ -> $2)
                                                        

}})

INST_BITS(Int,Bits32_,S)
INST_BITS(Int8,Bits8_,S)
INST_BITS(Int16,Bits16_,S)
INST_BITS(Int32,Bits32_,S)
INST_BITS(Int64,Bits64_,S)
INST_BITS(IntPtr,BitsPtr_,S)
INST_BITS(Integer,BitsMax_,S)

INST_BITS(Word,Bits32_)
INST_BITS(Word8,Bits8_)
INST_BITS(Word16,Bits16_)
INST_BITS(Word32,Bits32_)
INST_BITS(Word64,Bits64_)
INST_BITS(WordPtr,BitsPtr_)
INST_BITS(WordMax,BitsMax_)