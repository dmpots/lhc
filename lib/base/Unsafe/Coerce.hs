{-# OPTIONS_LHC -N -fffi #-}
module Unsafe.Coerce(unsafeCoerce) where


foreign import primitive unsafeCoerce :: a -> b
