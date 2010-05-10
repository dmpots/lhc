{-# LANGUAGE OverloadedStrings #-}
module LHC.Prim where
import CompactString
baseBuiltins :: [CompactString]
baseBuiltins = [
    "*#"
  , "*##"
  , "+#"
  , "+##"
  , "-#"
  , "-##"
  , "/##"
  , "/=#"
  , "<#"
  , "<##"
  , "<=#"
  , "<=##"
  , "==#"
  , "==##"
  , ">#"
  , ">##"
  , ">=#"
  , ">=##"
  , "Addr#"
  , "Array#"
  , "BCO#"
  , "ByteArray#"
  , "Char#"
  , "Double#"
  , "Float#"
  , "Int#"
  , "MVar#"
  , "MutVar#"
  , "MutableArray#"
  , "MutableByteArray#"
  , "StableName#"
  , "StablePtr#"
  , "State#"
  , "TVar#"
  , "ThreadId#"
  , "Weak#"
  , "Word#"
  , "acosDouble#"
  , "acosFloat#"
  , "addr2Int#"
  , "and#"
  , "asinDouble#"
  , "asinFloat#"
  , "atanDouble#"
  , "atanFloat#"
  , "byteArrayContents#"
  , "chr#"
  , "cmpInteger#"
  , "cmpIntegerInt#"
  , "cosDouble#"
  , "cosFloat#"
  , "coshDouble#"
  , "coshFloat#"
  , "dataToTag#"
  , "delay#"
  , "divideFloat#"
  , "double2Float#"
  , "double2Int#"
  , "eqAddr#"
  , "eqChar#"
  , "eqFloat#"
  , "eqStableName#"
  , "eqStablePtr#"
  , "eqWord#"
  , "expDouble#"
  , "expFloat#"
  , "float2Double#"
  , "float2Int#"
  , "gcdInt#"
  , "gcdIntegerInt#"
  , "geAddr#"
  , "geChar#"
  , "geFloat#"
  , "geWord#"
  , "gtAddr#"
  , "gtChar#"
  , "gtFloat#"
  , "gtWord#"
  , "indexAddrArray#"
  , "indexAddrOffAddr#"
  , "indexCharArray#"
  , "indexCharOffAddr#"
  , "indexDoubleArray#"
  , "indexDoubleOffAddr#"
  , "indexFloatArray#"
  , "indexFloatOffAddr#"
  , "indexInt16Array#"
  , "indexInt16OffAddr#"
  , "indexInt32Array#"
  , "indexInt32OffAddr#"
  , "indexInt64Array#"
  , "indexInt64OffAddr#"
  , "indexInt8Array#"
  , "indexInt8OffAddr#"
  , "indexIntArray#"
  , "indexIntOffAddr#"
  , "indexStablePtrArray#"
  , "indexStablePtrOffAddr#"
  , "indexWideCharArray#"
  , "indexWideCharOffAddr#"
  , "indexWord16Array#"
  , "indexWord16OffAddr#"
  , "indexWord32Array#"
  , "indexWord32OffAddr#"
  , "indexWord64Array#"
  , "indexWord64OffAddr#"
  , "indexWord8Array#"
  , "indexWord8OffAddr#"
  , "indexWordArray#"
  , "indexWordOffAddr#"
  , "int2Addr#"
  , "int2Double#"
  , "int2Float#"
  , "int2Word#"
  , "integer2Int#"
  , "integer2Word#"
  , "killThread#"
  , "labelThread#"
  , "leAddr#"
  , "leChar#"
  , "leFloat#"
  , "leWord#"
  , "logDouble#"
  , "logFloat#"
  , "ltAddr#"
  , "ltChar#"
  , "ltFloat#"
  , "ltWord#"
  , "minusAddr#"
  , "minusFloat#"
  , "minusWord#"
  , "mulIntMayOflo#"
  , "narrow16Int#"
  , "narrow16Word#"
  , "narrow32Int#"
  , "narrow32Word#"
  , "narrow8Int#"
  , "narrow8Word#"
  , "neAddr#"
  , "neChar#"
  , "neFloat#"
  , "neWord#"
  , "negateDouble#"
  , "negateFloat#"
  , "negateInt#"
  , "noDuplicate#"
  , "not#"
  , "nullAddr#"
  , "or#"
  , "ord#"
  , "par#"
  , "parAt#"
  , "parAtAbs#"
  , "parAtForNow#"
  , "parAtRel#"
  , "parGlobal#"
  , "parLocal#"
  , "plusAddr#"
  , "plusFloat#"
  , "plusWord#"
  , "powerFloat#"
  , "putMVar#"
  , "quotInt#"
  , "quotWord#"
  , "realWorld#"
  , "reallyUnsafePtrEquality#"
  , "remAddr#"
  , "remInt#"
  , "remWord#"
  , "sameMVar#"
  , "sameMutVar#"
  , "sameMutableArray#"
  , "sameMutableByteArray#"
  , "sameTVar#"
  , "sinDouble#"
  , "sinFloat#"
  , "sinhDouble#"
  , "sinhFloat#"
  , "sizeofByteArray#"
  , "sizeofMutableByteArray#"
  , "sqrtDouble#"
  , "sqrtFloat#"
  , "stableNameToInt#"
  , "tagToEnum#"
  , "tanDouble#"
  , "tanFloat#"
  , "tanhDouble#"
  , "tanhFloat#"
  , "timesFloat#"
  , "timesWord#"
  , "touch#"
  , "uncheckedIShiftL#"
  , "uncheckedIShiftRA#"
  , "uncheckedIShiftRL#"
  , "uncheckedShiftL#"
  , "uncheckedShiftRL#"
  , "unsafeCoerce#"
  , "waitRead#"
  , "waitWrite#"
  , "word2Int#"
  , "writeAddrArray#"
  , "writeAddrOffAddr#"
  , "writeCharArray#"
  , "writeCharOffAddr#"
  , "writeDoubleArray#"
  , "writeDoubleOffAddr#"
  , "writeFloatArray#"
  , "writeFloatOffAddr#"
  , "writeInt16Array#"
  , "writeInt16OffAddr#"
  , "writeInt32Array#"
  , "writeInt32OffAddr#"
  , "writeInt64Array#"
  , "writeInt64OffAddr#"
  , "writeInt8Array#"
  , "writeInt8OffAddr#"
  , "writeIntArray#"
  , "writeIntOffAddr#"
  , "writeMutVar#"
  , "writeStablePtrArray#"
  , "writeStablePtrOffAddr#"
  , "writeWideCharArray#"
  , "writeWideCharOffAddr#"
  , "writeWord16Array#"
  , "writeWord16OffAddr#"
  , "writeWord32Array#"
  , "writeWord32OffAddr#"
  , "writeWord64Array#"
  , "writeWord64OffAddr#"
  , "writeWord8Array#"
  , "writeWord8OffAddr#"
  , "writeWordArray#"
  , "writeWordOffAddr#"
  , "xor#"
  , "yield#"
  ]
vectorBuiltins :: [(CompactString, Int)]
vectorBuiltins = [
    ("addIntC#",2)
  , ("addrToHValue#",1)
  , ("andInteger#",2)
  , ("asyncExceptionsBlocked#",2)
  , ("catch#",2)
  , ("catchRetry#",2)
  , ("catchSTM#",2)
  , ("check#",2)
  , ("complementInteger#",2)
  , ("deRefWeak#",3)
  , ("decodeDouble#",3)
  , ("decodeDouble_2Int#",4)
  , ("decodeFloat#",3)
  , ("decodeFloat_Int#",2)
  , ("divExactInteger#",2)
  , ("divModInteger#",4)
  , ("finalizeWeak#",2)
  , ("forkOn#",2)
  , ("gcdInteger#",2)
  , ("getApStackVal#",2)
  , ("int2Integer#",2)
  , ("isCurrentThreadBound#",2)
  , ("isEmptyMVar#",2)
  , ("makeStableName#",2)
  , ("minusInteger#",2)
  , ("mkApUpd0#",1)
  , ("mkWeak#",2)
  , ("mkWeakForeignEnv#",2)
  , ("myThreadId#",2)
  , ("newAlignedPinnedByteArray#",2)
  , ("newBCO#",2)
  , ("newByteArray#",2)
  , ("newMVar#",2)
  , ("newMutVar#",2)
  , ("newPinnedByteArray#",2)
  , ("newTVar#",2)
  , ("orInteger#",2)
  , ("plusInteger#",2)
  , ("quotInteger#",2)
  , ("quotRemInteger#",4)
  , ("readAddrArray#",2)
  , ("readAddrOffAddr#",2)
  , ("readCharArray#",2)
  , ("readCharOffAddr#",2)
  , ("readDoubleArray#",2)
  , ("readDoubleOffAddr#",2)
  , ("readFloatArray#",2)
  , ("readFloatOffAddr#",2)
  , ("readInt16Array#",2)
  , ("readInt16OffAddr#",2)
  , ("readInt32Array#",2)
  , ("readInt32OffAddr#",2)
  , ("readInt64Array#",2)
  , ("readInt64OffAddr#",2)
  , ("readInt8Array#",2)
  , ("readInt8OffAddr#",2)
  , ("readIntArray#",2)
  , ("readIntOffAddr#",2)
  , ("readMutVar#",2)
  , ("readStablePtrArray#",2)
  , ("readStablePtrOffAddr#",2)
  , ("readTVar#",2)
  , ("readWideCharArray#",2)
  , ("readWideCharOffAddr#",2)
  , ("readWord16Array#",2)
  , ("readWord16OffAddr#",2)
  , ("readWord32Array#",2)
  , ("readWord32OffAddr#",2)
  , ("readWord64Array#",2)
  , ("readWord64OffAddr#",2)
  , ("readWord8Array#",2)
  , ("readWord8OffAddr#",2)
  , ("readWordArray#",2)
  , ("readWordOffAddr#",2)
  , ("remInteger#",2)
  , ("retry#",2)
  , ("subIntC#",2)
  , ("takeMVar#",2)
  , ("threadStatus#",2)
  , ("timesInteger#",2)
  , ("tryPutMVar#",2)
  , ("tryTakeMVar#",3)
  , ("unpackClosure#",3)
  , ("unsafeFreezeByteArray#",2)
  , ("unsafeThawArray#",2)
  , ("word2Integer#",2)
  , ("xorInteger#",2)
  ]
unsupportedBuiltins :: [CompactString]
unsupportedBuiltins = [
    "atomicModifyMutVar#"
  , "atomically#"
  , "fork#"
  , "raise#"
  , "raiseIO#"
  , "writeTVar#"
  ]
{- ORIGINAL DEFINITIONS
baseBuiltins        = ["<#",">#","<=#",">=#","-#","+#","*#","narrow32Int#"
                      ,"uncheckedIShiftRA#","and#","==#", "remInt#", "noDuplicate#"
                      ,"narrow8Word#", "writeInt8OffAddr#", "writeWord8OffAddr#"
                      ,"narrow8Int#", "byteArrayContents#","touch#"
                      ,"uncheckedIShiftL#", "negateInt#", "not#"
                      ,"indexCharOffAddr#","minusWord#","geWord#","eqWord#","narrow16Word#"
                      ,"neWord#", "ltWord#", "gtWord#", "remWord#"
                      ,"ord#","chr#","or#","narrow32Word#","uncheckedShiftL#","plusWord#"
                      ,"uncheckedShiftRL#","neChar#","narrow16Int#","timesWord#"
                      ,"writeAddrOffAddr#","writeInt32OffAddr#","quotInt#", "quotWord#"
                      ,"writeDoubleOffAddr#"
                      ,"readWord32OffAddr#"
                      ,"writeWord32OffAddr#"
                      ,"waitRead#"
                      ,"leWord#","/=#","writeCharArray#","xor#", "realWorld#"
                      ,"waitWrite#", "negateDouble#", "negateFloat#", "sqrtDouble#", "expDouble#", "**##"
                      ,"sinDouble#", "tanDouble#", "cosDouble#", "asinDouble#", "atanDouble#"
                      ,"acosDouble#", "asinhDouble#", "sinhDouble#", "tanhDouble#", "coshDouble#"
                      ,"<##", "==##", ">##", "<=##", ">=##", "-##", "+##", "*##", "/##"
                      ,"ltFloat#", "eqFloat#", "writeWord8Array#"
                      ,"coerceDoubleToWord", "coerceWordToDouble", "logDouble#", "int2Double#", "double2Int#"
                      ,"int2Float#", "divideFloat#", "timesFloat#", "minusFloat#", "plusFloat#"
                      ,"gtFloat#", "geFloat#", "leFloat#", "sqrtFloat#" ]
vectorBuiltins      = ["unsafeFreezeByteArray#", "newAlignedPinnedByteArray#"
                      , "word2Integer#","integer2Int#", "newByteArray#", "newPinnedByteArray#"
                      ,"readInt8OffAddr#","readInt32OffAddr#","readAddrOffAddr#","readInt32OffAddr#"
                      ,"readWord8Array#", "readDoubleOffAddr#", "writeDoubleOffAddr#"
                      ,"mkWeak#", "readCharArray#"]
unsupportedBuiltins = ["raise#","atomicModifyMutVar#","writeTVar#"
                      ,"raiseIO#","fork#","atomically#"]
-}
