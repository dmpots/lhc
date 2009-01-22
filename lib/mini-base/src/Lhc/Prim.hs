{-# OPTIONS_LHC -N -fffi -funboxed-tuples #-}
module Lhc.Prim where

-- this module is always included in all programs compiled by lhc. it defines some things that are needed to make lhc work at all.

import Lhc.String
import Lhc.Types

infixr 5  :
data [] a =  a : ([] a) | []

newtype IO a = IO (World__ -> (# World__, a #))

data World__ :: #

data Int   = Int   Bits32_
data Int8  = Int8  Bits8_
data Int16 = Int16 Bits16_
data Int32 = Int32 Bits32_
data Int64 = Int64 Bits64_
data IntPtr = IntPtr BitsPtr_

data Word = Word Word__
data WordMax = WordMax BitsMax_
data Word8  = Word8 Bits8_
data Word16 = Word16 Bits16_
data Word32 = Word32 Bits32_
data Word64 = Word64 Bits64_
data WordPtr = WordPtr BitsPtr_
data Char = Char Char__

type Bool__ = Bits16_ -- Change to Bits1_ when the time comes
type Addr__ = BitsPtr_
type Int__  = Bits32_
type Word__ = Bits32_
type Char__ = Bits32_
type Enum__ = Bits16_


-- | this is wrapped around arbitrary expressions and just evaluates them to whnf
foreign import primitive "seq" runRaw :: a -> World__ -> World__

-- | when no exception wrapper is wanted
runNoWrapper :: IO a -> World__ -> World__
runNoWrapper (IO run) w = case run w of (# w, _ #) -> w


foreign import primitive "unsafeCoerce" unsafeCoerce__ :: a -> b

-- like 'const' but creates an artificial dependency on its second argument to guide optimization.
foreign import primitive dependingOn :: forall a b. a -> b -> a
