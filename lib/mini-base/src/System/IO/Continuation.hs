module System.IO.Continuation(IOCont(),newContinuation,callContinuation) where

import Lhc.JumpPoint
import Lhc.Hole

data IOCont s a = IOCont (Hole a) JumpPoint

newContinuation :: (forall s . IOCont s a -> IO b) -> (a -> IO b) -> IO b
newContinuation act cc = do
    ref <- newHole
    withJumpPoint__ $ \jp r -> case r of
        False -> do act (IOCont ref jp)
        True  -> do readHole ref >>= cc

callContinuation :: IOCont s a -> a -> IO b
callContinuation (IOCont ref jp) x = do
    fillHole ref x
    jumpJumpPoint__ jp

