{-# OPTIONS_JHC -N -fffi -funboxed-tuples #-}
module Jhc.Prim where

-- this module is always included in all programs compiled by jhc. it defines some things that are needed to make jhc work at all.

import Jhc.String
import Jhc.Types

infixr 5  :
data [] a =  a : ([] a) | []

newtype IO a = IO (World__ -> (# World__, a #))

data World__ :: #

data Int
data Char

type Bool__ = Bits1_
type Addr__ = BitsPtr_
type Int__  = Bits32_
type Char__ = Bits32_


-- | this is wrapped around arbitrary expressions and just evaluates them to whnf
foreign import primitive "seq" runRaw :: a -> World__ -> World__

-- | when no exception wrapper is wanted
runNoWrapper :: IO a -> World__ -> World__
runNoWrapper (IO run) w = case run w of (# w, _ #) -> w


foreign import primitive "unsafeCoerce" unsafeCoerce__ :: a -> b

-- like 'const' but creates an artificial dependency on its second argument to guide optimization.
foreign import primitive dependingOn :: forall a b. a -> b -> a
