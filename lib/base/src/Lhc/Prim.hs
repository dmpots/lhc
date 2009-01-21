{-# OPTIONS_LHC -N -fffi -funboxed-tuples #-}
module Lhc.Prim where

-- this module is always included in all programs compiled by lhc. it defines some things that are needed to make lhc work at all.

import Lhc.String
import Lhc.Types

infixr 5  :
data [] a =  a : ([] a) | []

newtype IO a = IO (World__ -> (# World__, a #))

data World__ :: #

data Int  = Int  Int__
data Char = Char Char__

type Bool__ = Bits16_ -- Change to Bits1_ when the time comes
type Addr__ = BitsPtr_
type Int__  = Bits32_
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
