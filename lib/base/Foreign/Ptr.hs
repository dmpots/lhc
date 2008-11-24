{-# OPTIONS_LHC -N -fffi -funboxed-tuples #-}

module Foreign.Ptr(
    Ptr(),
    nullPtr,
    castPtr,
    plusPtr,
    alignPtr,
    minusPtr,
    FunPtr(),
    nullFunPtr,
    castFunPtr,
    castFunPtrToPtr,
    castPtrToFunPtr,
    freeHaskellFunPtr
    ) where


import Lhc.Show
import Lhc.Inst.Show
import Lhc.Monad
import Lhc.Order
import Lhc.IO
import Lhc.Basics
import Lhc.Num
import Lhc.Addr
import Foreign.Storable


instance Show (Ptr a) where
    showsPrec n (Ptr x) = showsPrec n (toInteger (addrToWordPtr  x))

nullPtr :: Ptr a
nullPtr = Ptr nullAddr

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr addr) off = Ptr (plusAddr addr off)

minusPtr :: Ptr a -> Int -> Ptr b
minusPtr (Ptr addr) off = Ptr (plusAddr addr (negate off))


alignPtr :: Ptr a -> Int -> Ptr a
alignPtr = error "alignPtr"
--alignPtr addr@(Ptr a) (I# i)
--  = case remAddr# a i of {
--      0# -> addr;
--      n -> Ptr (plusAddr# a (i -# n)) }



nullFunPtr = FunPtr nullFunAddr
castFunPtr (FunPtr addr) = FunPtr addr

--castFunPtrToPtr :: FunPtr a -> Ptr b
--castFunPtrToPtr = unsafeCoerce

--castPtrToFunPtr :: Ptr a -> FunPtr b
--castPtrToFunPtr = unsafeCoerce


foreign import primitive "U2U" castFunPtrToPtr :: FunPtr a -> Ptr b
foreign import primitive "U2U" castPtrToFunPtr :: Ptr a -> FunPtr b


