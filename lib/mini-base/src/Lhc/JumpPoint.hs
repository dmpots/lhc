{-# OPTIONS_LHC -fffi #-}
module Lhc.JumpPoint(JumpPoint(), withJumpPoint__, jumpJumpPoint__, errorJumpPoint) where

import Lhc.IO
import Lhc.Addr

newtype JumpPoint = JumpPoint Addr


-- | in order to be safe, the JumpPoint must not escape the handling function
withJumpPoint__ :: (JumpPoint -> Bool -> IO a) -> IO a
withJumpPoint__ action = do
    p <- _malloc jmp_buf_size
    let jp = (JumpPoint p)
    r <- lhc_setjmp jp
    r <- action jp (r /= 0)
    _free p
    return r

jumpJumpPoint__ :: JumpPoint -> IO a
jumpJumpPoint__ jp = lhc_longjmp  jp >> return (error "jumpJumpPoint__")

-- | jumping to this jumppoint will always abort the program.
foreign import ccall "&lhc_uncaught" errorJumpPoint :: JumpPoint

foreign import ccall lhc_setjmp :: JumpPoint -> IO Int
foreign import ccall lhc_longjmp :: JumpPoint -> IO ()
foreign import primitive "const.sizeof(jmp_buf)" jmp_buf_size  :: Int
foreign import ccall "malloc.h malloc" _malloc :: Int -> IO Addr
foreign import ccall "malloc.h free" _free :: Addr -> IO ()



