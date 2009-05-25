{-# OPTIONS_GHC -XNoImplicitPrelude -#include "HsBase.h" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Error
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- C-specific Marshalling support: Handling of C \"errno\" error codes.
--
-----------------------------------------------------------------------------

module Foreign.C.Error (

  -- * Haskell representations of @errno@ values

  Errno(..),            -- instance: Eq

  -- ** Common @errno@ symbols
  -- | Different operating systems and\/or C libraries often support
  -- different values of @errno@.  This module defines the common values,
  -- but due to the open definition of 'Errno' users may add definitions
  -- which are not predefined.
  eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eAFNOSUPPORT, eAGAIN,
  eALREADY, eBADF, eBADMSG, eBUSY, eCHILD, eCONNABORTED,
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDOM, eDQUOT,
  eEXIST, eFAULT, eFBIG, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ,
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK,
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH,
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK,
  eNOMEM, eNOMSG, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS,
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO,
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE,
  ePROTO, ePROTONOSUPPORT, ePROTOTYPE,
  eRANGE, eREMOTE, eROFS, eSHUTDOWN,
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSTALE, eTIME, eTIMEDOUT,
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV,
  {- unused; may go back in somehow?
  eADV, eCOMM, eNONET, eREMCHG, eSRMNT,
  -}
  -- ** 'Errno' functions
                        -- :: Errno
  isValidErrno,         -- :: Errno -> Bool

  -- access to the current thread's "errno" value
  --
  getErrno,             -- :: IO Errno
  resetErrno,           -- :: IO ()

  -- conversion of an "errno" value into IO error
  --
  errnoToIOError,       -- :: String       -- location
                        -- -> Errno        -- errno
                        -- -> Maybe Handle -- handle
                        -- -> Maybe String -- filename
                        -- -> IOError

  -- throw current "errno" value
  --
  throwErrno,           -- ::                String               -> IO a

  -- ** Guards for IO operations that may fail

  throwErrnoIf,         -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIf_,        -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfRetry,    -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIfRetry_,   -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfMinus1,   -- :: Num a
                        -- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1_,  -- :: Num a
                        -- =>                String -> IO a       -> IO ()
  throwErrnoIfMinus1Retry,
                        -- :: Num a
                        -- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1Retry_,
                        -- :: Num a
                        -- =>                String -> IO a       -> IO ()
  throwErrnoIfNull,     -- ::                String -> IO (Ptr a) -> IO (Ptr a)
  throwErrnoIfNullRetry,-- ::                String -> IO (Ptr a) -> IO (Ptr a)

  throwErrnoIfRetryMayBlock,
  throwErrnoIfRetryMayBlock_,
  throwErrnoIfMinus1RetryMayBlock,
  throwErrnoIfMinus1RetryMayBlock_,
  throwErrnoIfNullRetryMayBlock,

  throwErrnoPath,
  throwErrnoPathIf,
  throwErrnoPathIf_,
  throwErrnoPathIfNull,
  throwErrnoPathIfMinus1,
  throwErrnoPathIfMinus1_,

  sEEK_CUR,
  sEEK_SET,
  sEEK_END,
) where


-- this is were we get the CONST_XXX definitions from that configure
-- calculated for us
--
#ifndef __NHC__
#endif

#define __LHC__ 1
#define __GLASGOW_HASKELL__ 1

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Error    ( void )
import Data.Maybe

#if __GLASGOW_HASKELL__
import GHC.IOBase
import GHC.Num
import GHC.Base
#if __LHC__
import Foreign.Storable         ( Storable(poke,peek) )
#endif
#elif __HUGS__
import Hugs.Prelude             ( Handle, IOError, ioError )
import System.IO.Unsafe         ( unsafePerformIO )
#else
import System.IO                ( Handle )
import System.IO.Error          ( IOError, ioError )
import System.IO.Unsafe         ( unsafePerformIO )
import Foreign.Storable         ( Storable(poke,peek) )
#endif

#ifdef __HUGS__
{-# CFILES cbits/PrelIOUtils.c #-}
#endif


-- FIXME: These shouldn't be defined here.
sEEK_CUR :: CInt
sEEK_CUR = #{const SEEK_CUR}

sEEK_SET :: CInt
sEEK_SET = #{const SEEK_SET}

sEEK_END :: CInt
sEEK_END = #{const SEEK_END}


-- "errno" type
-- ------------

-- | Haskell representation for @errno@ values.
-- The implementation is deliberately exposed, to allow users to add
-- their own definitions of 'Errno' values.

newtype Errno = Errno CInt

instance Eq Errno where
  errno1@(Errno no1) == errno2@(Errno no2)
    | isValidErrno errno1 && isValidErrno errno2 = no1 == no2
    | otherwise                                  = False

-- common "errno" symbols
--
eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eAFNOSUPPORT, eAGAIN,
  eALREADY, eBADF, eBADMSG, eBUSY, eCHILD, eCONNABORTED,
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDOM, eDQUOT,
  eEXIST, eFAULT, eFBIG, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ,
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK,
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH,
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK,
  eNOMEM, eNOMSG, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS,
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO,
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE,
  ePROTO, ePROTONOSUPPORT, ePROTOTYPE,
  eRANGE, eREMOTE, eROFS, eSHUTDOWN,
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSTALE, eTIME, eTIMEDOUT,
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV                    :: Errno
{- eADV, eCOMM, eNONET, eREMCHG, eSRMNT :: Errno -}
--
-- the cCONST_XXX identifiers are cpp symbols whose value is computed by
-- configure
--
eOK             = Errno 0
#ifdef __NHC__
#include "Errno.hs"
#else
#include "errno.h"
e2BIG           = Errno #{const E2BIG}
eACCES          = Errno #{const EACCES}
eADDRINUSE      = Errno #{const EADDRINUSE}
eADDRNOTAVAIL   = Errno #{const EADDRNOTAVAIL}
--eADV            = Errno #{const EADV}
eAFNOSUPPORT    = Errno #{const EAFNOSUPPORT}
eAGAIN          = Errno #{const EAGAIN}
eALREADY        = Errno #{const EALREADY}
eBADF           = Errno #{const EBADF}
eBADMSG         = Errno #{const EBADMSG}
--eBADRPC         = Errno #{const EBADRPC}
eBUSY           = Errno #{const EBUSY}
eCHILD          = Errno #{const ECHILD}
--eCOMM           = Errno #{const ECOMM}
eCONNABORTED    = Errno #{const ECONNABORTED}
eCONNREFUSED    = Errno #{const ECONNREFUSED}
eCONNRESET      = Errno #{const ECONNRESET}
eDEADLK         = Errno #{const EDEADLK}
eDESTADDRREQ    = Errno #{const EDESTADDRREQ}
--eDIRTY          = Errno #{const EDIRTY}
eDOM            = Errno #{const EDOM}
eDQUOT          = Errno #{const EDQUOT}
eEXIST          = Errno #{const EEXIST}
eFAULT          = Errno #{const EFAULT}
eFBIG           = Errno #{const EFBIG}
--eFTYPE          = Errno #{const EFTYPE}
eHOSTDOWN       = Errno #{const EHOSTDOWN}
eHOSTUNREACH    = Errno #{const EHOSTUNREACH}
eIDRM           = Errno #{const EIDRM}
eILSEQ          = Errno #{const EILSEQ}
eINPROGRESS     = Errno #{const EINPROGRESS}
eINTR           = Errno #{const EINTR}
eINVAL          = Errno #{const EINVAL}
eIO             = Errno #{const EIO}
eISCONN         = Errno #{const EISCONN}
eISDIR          = Errno #{const EISDIR}
eLOOP           = Errno #{const ELOOP}
eMFILE          = Errno #{const EMFILE}
eMLINK          = Errno #{const EMLINK}
eMSGSIZE        = Errno #{const EMSGSIZE}
eMULTIHOP       = Errno #{const EMULTIHOP}
eNAMETOOLONG    = Errno #{const ENAMETOOLONG}
eNETDOWN        = Errno #{const ENETDOWN}
eNETRESET       = Errno #{const ENETRESET}
eNETUNREACH     = Errno #{const ENETUNREACH}
eNFILE          = Errno #{const ENFILE}
eNOBUFS         = Errno #{const ENOBUFS}
eNODATA         = Errno #{const ENODATA}
eNODEV          = Errno #{const ENODEV}
eNOENT          = Errno #{const ENOENT}
eNOEXEC         = Errno #{const ENOEXEC}
eNOLCK          = Errno #{const ENOLCK}
eNOLINK         = Errno #{const ENOLINK}
eNOMEM          = Errno #{const ENOMEM}
eNOMSG          = Errno #{const ENOMSG}
--eNONET          = Errno #{const ENONET}
eNOPROTOOPT     = Errno #{const ENOPROTOOPT}
eNOSPC          = Errno #{const ENOSPC}
eNOSR           = Errno #{const ENOSR}
eNOSTR          = Errno #{const ENOSTR}
eNOSYS          = Errno #{const ENOSYS}
eNOTBLK         = Errno #{const ENOTBLK}
eNOTCONN        = Errno #{const ENOTCONN}
eNOTDIR         = Errno #{const ENOTDIR}
eNOTEMPTY       = Errno #{const ENOTEMPTY}
eNOTSOCK        = Errno #{const ENOTSOCK}
eNOTTY          = Errno #{const ENOTTY}
eNXIO           = Errno #{const ENXIO}
eOPNOTSUPP      = Errno #{const EOPNOTSUPP}
ePERM           = Errno #{const EPERM}
ePFNOSUPPORT    = Errno #{const EPFNOSUPPORT}
ePIPE           = Errno #{const EPIPE}
--ePROCLIM        = Errno #{const EPROCLIM}
--ePROCUNAVAIL    = Errno #{const EPROCUNAVAIL}
--ePROGMISMATCH   = Errno #{const EPROGMISMATCH}
--ePROGUNAVAIL    = Errno #{const EPROGUNAVAIL}
ePROTO          = Errno #{const EPROTO}
ePROTONOSUPPORT = Errno #{const EPROTONOSUPPORT}
ePROTOTYPE      = Errno #{const EPROTOTYPE}
eRANGE          = Errno #{const ERANGE}
--eREMCHG         = Errno #{const EREMCHG}
eREMOTE         = Errno #{const EREMOTE}
eROFS           = Errno #{const EROFS}
--eRPCMISMATCH    = Errno #{const ERPCMISMATCH}
--eRREMOTE        = Errno #{const ERREMOTE}
eSHUTDOWN       = Errno #{const ESHUTDOWN}
eSOCKTNOSUPPORT = Errno #{const ESOCKTNOSUPPORT}
eSPIPE          = Errno #{const ESPIPE}
eSRCH           = Errno #{const ESRCH}
--eSRMNT          = Errno #{const ESRMNT}
eSTALE          = Errno #{const ESTALE}
eTIME           = Errno #{const ETIME}
eTIMEDOUT       = Errno #{const ETIMEDOUT}
eTOOMANYREFS    = Errno #{const ETOOMANYREFS}
eTXTBSY         = Errno #{const ETXTBSY}
eUSERS          = Errno #{const EUSERS}
eWOULDBLOCK     = Errno #{const EWOULDBLOCK}
eXDEV           = Errno #{const EXDEV}
#endif

-- | Yield 'True' if the given 'Errno' value is valid on the system.
-- This implies that the 'Eq' instance of 'Errno' is also system dependent
-- as it is only defined for valid values of 'Errno'.
--
isValidErrno               :: Errno -> Bool
--
-- the configure script sets all invalid "errno"s to -1
--
isValidErrno (Errno errno)  = errno /= -1


-- access to the current thread's "errno" value
-- --------------------------------------------

-- | Get the current value of @errno@ in the current thread.
--
getErrno :: IO Errno

-- We must call a C function to get the value of errno in general.  On
-- threaded systems, errno is hidden behind a C macro so that each OS
-- thread gets its own copy.
#if defined(__NHC__)
getErrno = do e <- peek _errno; return (Errno e)
foreign import ccall unsafe "errno.h &errno" _errno :: Ptr CInt
#else
getErrno = do e <- get_errno; return (Errno e)
foreign import ccall unsafe "HsBase.h __hscore_get_errno" get_errno :: IO CInt
#endif

-- | Reset the current thread\'s @errno@ value to 'eOK'.
--
resetErrno :: IO ()

-- Again, setting errno has to be done via a C function.
#if defined(__NHC__)
resetErrno = poke _errno 0
#else
resetErrno = set_errno 0
foreign import ccall unsafe "HsBase.h __hscore_set_errno" set_errno :: CInt -> IO ()
#endif

-- throw current "errno" value
-- ---------------------------

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'.
--
throwErrno     :: String        -- ^ textual description of the error location
               -> IO a
throwErrno loc  =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing Nothing)


-- guards for IO operations that may fail
-- --------------------------------------

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the result value of the 'IO' action meets the given predicate.
--
throwErrnoIf    :: (a -> Bool)  -- ^ predicate to apply to the result value
                                -- of the 'IO' operation
                -> String       -- ^ textual description of the location
                -> IO a         -- ^ the 'IO' operation to be executed
                -> IO a
throwErrnoIf pred loc f  =
  do
    res <- f
    if pred res then throwErrno loc else return res

-- | as 'throwErrnoIf', but discards the result of the 'IO' action after
-- error handling.
--
throwErrnoIf_   :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ pred loc f  = void $ throwErrnoIf pred loc f

-- | as 'throwErrnoIf', but retry the 'IO' action when it yields the
-- error code 'eINTR' - this amounts to the standard retry loop for
-- interrupted POSIX system calls.
--
throwErrnoIfRetry            :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry pred loc f  =
  do
    res <- f
    if pred res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoIfRetry pred loc f
          else throwErrno loc
      else return res

-- | as 'throwErrnoIfRetry', but checks for operations that would block and
-- executes an alternative action before retrying in that case.
--
throwErrnoIfRetryMayBlock
                :: (a -> Bool)  -- ^ predicate to apply to the result value
                                -- of the 'IO' operation
                -> String       -- ^ textual description of the location
                -> IO a         -- ^ the 'IO' operation to be executed
                -> IO b         -- ^ action to execute before retrying if
                                -- an immediate retry would block
                -> IO a
throwErrnoIfRetryMayBlock pred loc f on_block  =
  do
    res <- f
    if pred res
      then do
        err <- getErrno
        if err == eINTR
          then throwErrnoIfRetryMayBlock pred loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
                 then do on_block; throwErrnoIfRetryMayBlock pred loc f on_block
                 else throwErrno loc
      else return res

-- | as 'throwErrnoIfRetry', but discards the result.
--
throwErrnoIfRetry_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ pred loc f  = void $ throwErrnoIfRetry pred loc f

-- | as 'throwErrnoIfRetryMayBlock', but discards the result.
--
throwErrnoIfRetryMayBlock_ :: (a -> Bool) -> String -> IO a -> IO b -> IO ()
throwErrnoIfRetryMayBlock_ pred loc f on_block
  = void $ throwErrnoIfRetryMayBlock pred loc f on_block

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the 'IO' action returns a result of @-1@.
--
throwErrnoIfMinus1 :: Num a => String -> IO a -> IO a
throwErrnoIfMinus1  = throwErrnoIf (== -1)

-- | as 'throwErrnoIfMinus1', but discards the result.
--
throwErrnoIfMinus1_ :: Num a => String -> IO a -> IO ()
throwErrnoIfMinus1_  = throwErrnoIf_ (== -1)

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the 'IO' action returns a result of @-1@, but retries in case of
-- an interrupted operation.
--
throwErrnoIfMinus1Retry :: Num a => String -> IO a -> IO a
throwErrnoIfMinus1Retry  = throwErrnoIfRetry (== -1)

-- | as 'throwErrnoIfMinus1', but discards the result.
--
throwErrnoIfMinus1Retry_ :: Num a => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_  = throwErrnoIfRetry_ (== -1)

-- | as 'throwErrnoIfMinus1Retry', but checks for operations that would block.
--
throwErrnoIfMinus1RetryMayBlock :: Num a => String -> IO a -> IO b -> IO a
throwErrnoIfMinus1RetryMayBlock  = throwErrnoIfRetryMayBlock (== -1)

-- | as 'throwErrnoIfMinus1RetryMayBlock', but discards the result.
--
throwErrnoIfMinus1RetryMayBlock_ :: Num a => String -> IO a -> IO b -> IO ()
throwErrnoIfMinus1RetryMayBlock_  = throwErrnoIfRetryMayBlock_ (== -1)

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the 'IO' action returns 'nullPtr'.
--
throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull  = throwErrnoIf (== nullPtr)

-- | Throw an 'IOError' corresponding to the current value of 'getErrno'
-- if the 'IO' action returns 'nullPtr',
-- but retry in case of an interrupted operation.
--
throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry  = throwErrnoIfRetry (== nullPtr)

-- | as 'throwErrnoIfNullRetry', but checks for operations that would block.
--
throwErrnoIfNullRetryMayBlock :: String -> IO (Ptr a) -> IO b -> IO (Ptr a)
throwErrnoIfNullRetryMayBlock  = throwErrnoIfRetryMayBlock (== nullPtr)

-- | as 'throwErrno', but exceptions include the given path when appropriate.
--
throwErrnoPath :: String -> FilePath -> IO a
throwErrnoPath loc path =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing (Just path))

-- | as 'throwErrnoIf', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIf pred loc path f =
  do
    res <- f
    if pred res then throwErrnoPath loc path else return res

-- | as 'throwErrnoIf_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIf_ :: (a -> Bool) -> String -> FilePath -> IO a -> IO ()
throwErrnoPathIf_ pred loc path f  = void $ throwErrnoPathIf pred loc path f

-- | as 'throwErrnoIfNull', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfNull :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull  = throwErrnoPathIf (== nullPtr)

-- | as 'throwErrnoIfMinus1', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1 :: Num a => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (== -1)

-- | as 'throwErrnoIfMinus1_', but exceptions include the given path when
--   appropriate.
--
throwErrnoPathIfMinus1_ :: Num a => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_  = throwErrnoPathIf_ (== -1)

-- conversion of an "errno" value into IO error
-- --------------------------------------------

-- | Construct a Haskell 98 I\/O error based on the given 'Errno' value.
-- The optional information can be used to improve the accuracy of
-- error messages.
--
errnoToIOError  :: String       -- ^ the location where the error occurred
                -> Errno        -- ^ the error number
                -> Maybe Handle -- ^ optional handle associated with the error
                -> Maybe String -- ^ optional filename associated with the error
                -> IOError
errnoToIOError loc errno maybeHdl maybeName = unsafePerformIO $ do
    str <- strerror errno >>= peekCString
#if __GLASGOW_HASKELL__
    return (IOError maybeHdl errType loc str maybeName)
    where
    errType
        | errno == eOK             = OtherError
        | errno == e2BIG           = ResourceExhausted
        | errno == eACCES          = PermissionDenied
        | errno == eADDRINUSE      = ResourceBusy
        | errno == eADDRNOTAVAIL   = UnsupportedOperation
--        | errno == eADV            = OtherError
        | errno == eAFNOSUPPORT    = UnsupportedOperation
        | errno == eAGAIN          = ResourceExhausted
        | errno == eALREADY        = AlreadyExists
        | errno == eBADF           = InvalidArgument
        | errno == eBADMSG         = InappropriateType
--        | errno == eBADRPC         = OtherError
        | errno == eBUSY           = ResourceBusy
        | errno == eCHILD          = NoSuchThing
--        | errno == eCOMM           = ResourceVanished
        | errno == eCONNABORTED    = OtherError
        | errno == eCONNREFUSED    = NoSuchThing
        | errno == eCONNRESET      = ResourceVanished
        | errno == eDEADLK         = ResourceBusy
        | errno == eDESTADDRREQ    = InvalidArgument
--        | errno == eDIRTY          = UnsatisfiedConstraints
        | errno == eDOM            = InvalidArgument
        | errno == eDQUOT          = PermissionDenied
        | errno == eEXIST          = AlreadyExists
        | errno == eFAULT          = OtherError
        | errno == eFBIG           = PermissionDenied
--        | errno == eFTYPE          = InappropriateType
        | errno == eHOSTDOWN       = NoSuchThing
        | errno == eHOSTUNREACH    = NoSuchThing
        | errno == eIDRM           = ResourceVanished
        | errno == eILSEQ          = InvalidArgument
        | errno == eINPROGRESS     = AlreadyExists
        | errno == eINTR           = Interrupted
        | errno == eINVAL          = InvalidArgument
        | errno == eIO             = HardwareFault
        | errno == eISCONN         = AlreadyExists
        | errno == eISDIR          = InappropriateType
        | errno == eLOOP           = InvalidArgument
        | errno == eMFILE          = ResourceExhausted
        | errno == eMLINK          = ResourceExhausted
        | errno == eMSGSIZE        = ResourceExhausted
        | errno == eMULTIHOP       = UnsupportedOperation
        | errno == eNAMETOOLONG    = InvalidArgument
        | errno == eNETDOWN        = ResourceVanished
        | errno == eNETRESET       = ResourceVanished
        | errno == eNETUNREACH     = NoSuchThing
        | errno == eNFILE          = ResourceExhausted
        | errno == eNOBUFS         = ResourceExhausted
        | errno == eNODATA         = NoSuchThing
        | errno == eNODEV          = UnsupportedOperation
        | errno == eNOENT          = NoSuchThing
        | errno == eNOEXEC         = InvalidArgument
        | errno == eNOLCK          = ResourceExhausted
        | errno == eNOLINK         = ResourceVanished
        | errno == eNOMEM          = ResourceExhausted
        | errno == eNOMSG          = NoSuchThing
--        | errno == eNONET          = NoSuchThing
        | errno == eNOPROTOOPT     = UnsupportedOperation
        | errno == eNOSPC          = ResourceExhausted
        | errno == eNOSR           = ResourceExhausted
        | errno == eNOSTR          = InvalidArgument
        | errno == eNOSYS          = UnsupportedOperation
        | errno == eNOTBLK         = InvalidArgument
        | errno == eNOTCONN        = InvalidArgument
        | errno == eNOTDIR         = InappropriateType
        | errno == eNOTEMPTY       = UnsatisfiedConstraints
        | errno == eNOTSOCK        = InvalidArgument
        | errno == eNOTTY          = IllegalOperation
        | errno == eNXIO           = NoSuchThing
        | errno == eOPNOTSUPP      = UnsupportedOperation
        | errno == ePERM           = PermissionDenied
        | errno == ePFNOSUPPORT    = UnsupportedOperation
        | errno == ePIPE           = ResourceVanished
--        | errno == ePROCLIM        = PermissionDenied
--        | errno == ePROCUNAVAIL    = UnsupportedOperation
--        | errno == ePROGMISMATCH   = ProtocolError
--        | errno == ePROGUNAVAIL    = UnsupportedOperation
        | errno == ePROTO          = ProtocolError
        | errno == ePROTONOSUPPORT = ProtocolError
        | errno == ePROTOTYPE      = ProtocolError
        | errno == eRANGE          = UnsupportedOperation
--        | errno == eREMCHG         = ResourceVanished
        | errno == eREMOTE         = IllegalOperation
        | errno == eROFS           = PermissionDenied
--        | errno == eRPCMISMATCH    = ProtocolError
--        | errno == eRREMOTE        = IllegalOperation
        | errno == eSHUTDOWN       = IllegalOperation
        | errno == eSOCKTNOSUPPORT = UnsupportedOperation
        | errno == eSPIPE          = UnsupportedOperation
        | errno == eSRCH           = NoSuchThing
--        | errno == eSRMNT          = UnsatisfiedConstraints
        | errno == eSTALE          = ResourceVanished
        | errno == eTIME           = TimeExpired
        | errno == eTIMEDOUT       = TimeExpired
        | errno == eTOOMANYREFS    = ResourceExhausted
        | errno == eTXTBSY         = ResourceBusy
        | errno == eUSERS          = ResourceExhausted
        | errno == eWOULDBLOCK     = OtherError
        | errno == eXDEV           = UnsupportedOperation
        | otherwise                = OtherError
#else
    return (userError (loc ++ ": " ++ str ++ maybe "" (": "++) maybeName))
#endif

foreign import ccall unsafe "string.h" strerror :: Errno -> IO (Ptr CChar)
