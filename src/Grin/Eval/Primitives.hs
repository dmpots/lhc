{-# LANGUAGE TypeSynonymInstances #-}
module Grin.Eval.Primitives
    ( runExternal
    , listPrimitives
    , realWorld
    ) where

import Grin.Types hiding (Value(..))
import qualified Grin.Types as Grin

import qualified Data.Map as Map

import CompactString
import Grin.Types hiding (Value(..))
import Grin.Eval.Types

import qualified Data.Map as Map
import Control.Monad.State
import Control.Exception
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C
import Foreign.Storable
import Data.Char; import Data.Word; import Data.Bits; import Data.Int

import Foreign.Marshal (copyBytes, newArray)
import System.Posix (fdWrite,Fd(..))
import System.Posix.DynamicLinker
import Foreign.LibFFI

import Grin.Eval.Methods

-- These functions are defined in the base library. I'm not sure how to deal with this properly.
runExternal :: String -> [CompValue] -> Gen CompValue
runExternal name args
    = do tNode <- getUnboxedT
         t0Node <- getUnboxedT0
         let returnIO v = return (mkNode tNode [realWorld, v])
         return $
            do args' <- mapM id args
               case (name, args') of
                 ("__hscore_memcpy_dst_off", [Lit (Lint dst),Lit (Lint off),Lit (Lint src),Lit (Lint size), realWorld]) ->
                   do let dstPtr = nullPtr `plusPtr` (fromIntegral (dst+off))
                          srcPtr = nullPtr `plusPtr` (fromIntegral src)
                      liftIO $ copyBytes dstPtr srcPtr (fromIntegral size)
                      returnIO (Lit (Lint (dst+off)))
                 ("__hscore_PrelHandle_write", [Lit (Lint fd),Lit (Lint ptr),Lit (Lint offset),Lit (Lint size),realWorld]) ->
                   do let strPtr = nullPtr `plusPtr` fromIntegral (ptr+offset)
                      str <- liftIO $ peekCStringLen (strPtr,fromIntegral size)
                      out <- liftIO $ fdWrite (Fd (fromIntegral fd)) str
                      returnIO (Lit (Lint $ fromIntegral out))
                 ("__hscore_get_errno", [realWorld]) ->
                   do returnIO (Lit (Lint 0))
                 ("__hscore_bufsiz", [realWorld]) ->
                   do returnIO (Lit (Lint 512))
                 ("fdReady", [fd,write,msecs,isSock,realWorld]) ->
                   do returnIO (Lit (Lint 1))
                 ("rtsSupportsBoundThreads", [realWorld]) ->
                   do returnIO (Lit (Lint 1))
                 ("stg_sig_install", [signo, actioncode, ptr, realWorld]) ->
                   do returnIO (Lit (Lint 0))
                 ("getProgArgv", [Lit (Lint argcPtr), Lit (Lint argvPtr), realWorld]) ->
                   do args <- getCommandArgs
                      liftIO $ poke (nullPtr `plusPtr` fromIntegral argcPtr) (fromIntegral (length args) :: CInt)
                      cs <- liftIO $ newArray =<< mapM newCString args
                      liftIO $ poke (nullPtr `plusPtr` fromIntegral argvPtr) cs
                      return $ mkNode t0Node [realWorld]
                 (name, args) ->
                   -- If we don't recognize the function, try loading it through the linker.
                   do fnPtr <- liftIO $ dlsym Default name
                      let toCArg (Lit (Lint i)) = argCInt (fromIntegral i)
                      ret <- liftIO $ callFFI fnPtr retCInt (map toCArg $ init args)
                      returnIO $ Lit (Lint (fromIntegral ret))







newtype IntArg = IntArg Int
newtype CharArg = CharArg Char
newtype ArrayArg = ArrayArg [EvalValue]
newtype AnyArg = AnyArg EvalValue
newtype PtrArg = PtrArg (Ptr ())
newtype HeapArg = HeapArg HeapPointer
data RealWorld = RealWorld

data Primitive = Primitive { primName :: String, primHandle :: ([EvalValue] -> CompExpression) }


-- Primitive handlers

listPrimitives :: GlobalScope -> [(Renamed, CompFunction)]
listPrimitives globalScope
    = [ (Builtin name, primHandle (prim globalScope)) | (name, prim) <- Map.toList allPrimitives ]

allPrimitives :: Map.Map CompactString (GlobalScope -> Primitive)
allPrimitives = Map.fromList [ (fromString name, prim) | (name, prim) <- prims ]
    where prims = [ equal, gt, lt, gte, lte, gtChar, geChar, ltChar, leChar
                     , plus, minus, times, remInt, quotInt, addIntC
                     , chrPrim, ordPrim
                     , indexCharOffAddr
                     , readInt32OffAddr, readInt8OffAddr, readAddrOffAddr
                     , writeCharArray
                     , plusAddr, touch
                     , noDuplicate
                     , realWorldPrim, myThreadIdPrim, raisePrim
                     , catchPrim, blockAsyncExceptions, unblockAsyncExceptions
                     , newPinnedByteArray, newAlignedPinnedByteArray
                     , unsafeFreezeByteArray, byteArrayContents
                     , updatePrim, evalPrim, applyPrim
                     , newArrayPrim, readArray, writeArray
                     , newMutVar, writeMutVar, readMutVar
                     , narrow8Word, narrow32Int, int2Word, word2Int, negateInt
                     , newMVar, putMVar, takeMVar
                     , mkWeak]



-- Primitive definitions

equal = mkPrimitive "==#" $ binOp (==)
gt    = mkPrimitive ">#" $ binOp (>)
lt    = mkPrimitive "<#" $ binOp (<)
gte   = mkPrimitive ">=#" $ binOp (>=)
lte   = mkPrimitive "<=#" $ binOp (<=)

gtChar = mkPrimitive "gtChar#" $ binOp (>)
geChar = mkPrimitive "geChar#" $ binOp (>=)
ltChar = mkPrimitive "ltChar#" $ binOp (<)
leChar = mkPrimitive "leChar#" $ binOp (<=)


plus = mkPrimitive "+#" $ binIntOp (+)
minus = mkPrimitive "-#" $ binIntOp (-)
times = mkPrimitive "*#" $ binIntOp (*)
remInt = mkPrimitive "remInt#" $ binIntOp rem
quotInt = mkPrimitive "quotInt#" $ binIntOp quot

addIntC = mkPrimitive "addIntC#" $
             do tNode <- getUnboxedT
                return $ \(IntArg a) (IntArg b) ->
                        let c = fromIntegral a + fromIntegral b
                            o = c `shiftR` bitSize (0::Int)
                        in noScope $ return (mkNode tNode [Lit (Lint c), Lit (Lint o)])
chrPrim
    = mkPrimitive "chr#" $
      return $ \(IntArg i) ->
                  noScope $ return $ Lit $ Lchar (chr i)
ordPrim
    = mkPrimitive "ord#" $
      return $ \(CharArg c) ->
                  noScope $ return $ Lit $ Lint (fromIntegral $ ord c)

-- |Reads 8-bit character; offset in bytes.
indexCharOffAddr
    = mkPrimitive "indexCharOffAddr#" $
      return $ \(PtrArg ptr) (IntArg nth) ->
                 noScope $ do c <- peekByteOff ptr nth :: IO Word8
                              return (Lit (Lchar (chr (fromIntegral (c::Word8)))))

readInt32OffAddr
    = mkPrimitive "readInt32OffAddr#" $
      do tNode <- getUnboxedT
         return $ \(PtrArg ptr) (IntArg nth) RealWorld ->
                    noScope $ do i <- peekElemOff (castPtr ptr) nth
                                 return (mkNode tNode [realWorld, fromInt (fromIntegral (i::Int32))])

readInt8OffAddr
    = mkPrimitive "readInt8OffAddr#" $
      do tNode <- getUnboxedT
         return $ \(PtrArg ptr) (IntArg nth) RealWorld ->
                   noScope $ do i <-  peekElemOff (castPtr ptr) nth
                                return $ mkNode tNode [realWorld, fromInt (fromIntegral (i::Int8))]

readAddrOffAddr
    = mkPrimitive "readAddrOffAddr#" $
      do tNode <- getUnboxedT
         return $ \(PtrArg ptr) (IntArg nth) RealWorld ->
                    noScope $ do p <- peekElemOff (castPtr ptr) nth
                                 return $ mkNode tNode [realWorld, fromPointer p]

-- |Write 8-bit character; offset in bytes.
writeCharArray
    = mkPrimitive "writeCharArray#" $
      do tNode <- getUnboxedT
         return $ \(PtrArg ptr) (IntArg offset) (CharArg c) RealWorld ->
                    noScope $ do poke (ptr `plusPtr` offset) (fromIntegral (ord c) :: Word8)
                                 return realWorld

plusAddr
    = mkPrimitive "plusAddr#" $ return $ \(PtrArg ptr) (IntArg offset) ->
      noScope $ return (fromPointer (ptr `plusPtr` offset))
touch
    = mkPrimitive "touch#" $ return $ \(AnyArg _) RealWorld ->
      noScope $ return realWorld

noDuplicate = mkPrimitive "noDuplicate#" $ return $ \RealWorld -> noScope $ return realWorld

realWorldPrim = mkPrimitive "realWorld#" $ return $ noScope $ return realWorld

myThreadIdPrim
    = mkPrimitive "myThreadId#" $
      do tNode <- getUnboxedT
         return $ \RealWorld ->
                     noScope $ return (mkNode tNode [realWorld, Lit (Lint 0)])

raisePrim
    = mkPrimitive "raise#" $
      return $ \(HeapArg ptr) ->
               do st <- get
                  liftIO $ throwIO (GrinException st ptr) :: CompValue

catchPrim
    = mkPrimitive "catch#" $
      do apply <- lookupFunction (Builtin $ fromString "apply")
         return $ \(AnyArg fn) (AnyArg handler) RealWorld ->
                  apply [fn, realWorld] `catchComp` \val ->
                  do v <- apply [handler, val]
                     apply [v, realWorld]

blockAsyncExceptions
    = mkPrimitive "blockAsyncExceptions#" $
      do apply <- lookupFunction (Builtin $ fromString "apply")
         return $ \(AnyArg fn) RealWorld ->
                     apply [fn,realWorld]

unblockAsyncExceptions
    = mkPrimitive "unblockAsyncExceptions#" $
      do apply <- lookupFunction (Builtin $ fromString "apply")
         return $ \(AnyArg fn) RealWorld ->
                     apply [fn, realWorld]

-- |Create a mutable byte array that the GC guarantees not to move.
newPinnedByteArray
    = mkPrimitive "newPinnedByteArray#" $
      do tNode <- getUnboxedT
         return $ \(IntArg size) RealWorld ->
                    noScope $ do ptr <- mallocBytes size
                                 return (mkNode tNode [realWorld , fromPointer ptr])

newAlignedPinnedByteArray
    = mkPrimitive "newAlignedPinnedByteArray#" $
      do tNode <- getUnboxedT
         return $ \(IntArg size) (IntArg alignment) RealWorld ->
                    noScope $ do ptr <- mallocBytes (size + alignment)
                                 return (mkNode tNode [realWorld, fromPointer $ alignPtr ptr alignment])

unsafeFreezeByteArray
    = mkPrimitive "unsafeFreezeByteArray#" $
      do tNode <- getUnboxedT
         return $ \(PtrArg ptr) RealWorld ->
                     noScope $ return (mkNode tNode [realWorld, fromPointer ptr])

byteArrayContents
    = mkPrimitive "byteArrayContents#" $ return $ \(PtrArg ptr) ->
      noScope $ return (fromPointer ptr)

updatePrim
    = mkPrimitive "update" $ return $ \(HeapArg ptr) (AnyArg val) ->
      do updateValue ptr val
         return Empty

evalPrim
    = mkPrimitive "eval" $ return $ \(AnyArg arg) ->
      runEvalPrimitive arg

applyPrim
    = mkPrimitive "apply" $ return $ \(AnyArg fnPtr) (AnyArg arg) ->
      do fn <- runEvalPrimitive fnPtr
         case fn of
              FNode name fn 1 args -> fn (args ++ [arg])
              FNode name fn 0 args -> error $ "apply: over application?"
              FNode name fn n args -> return $ FNode name fn (n-1) (args ++ [arg])
              CNode name 0 args -> error $ "apply: over application?"
              CNode name n args -> return $ CNode name (n-1) (args ++ [arg])
              _ -> error $ "weird apply: " ++ (show fn)

newArrayPrim
    = mkPrimitive "newArray#" $
      do tNode <- getUnboxedT
         return $ \(IntArg len) (AnyArg elt) RealWorld ->
                    do ptr <- storeValue (Array $ replicate len elt)
                       return $ mkNode tNode [realWorld, HeapPointer ptr ]

readArray
    = mkPrimitive "readArray#" $
      do tNode <- getUnboxedT
         return $ \(HeapArg ptr) (IntArg idx) RealWorld ->
                    do Array arr <- fetch ptr
                       return $ mkNode tNode [realWorld, arr!!idx]

writeArray
    = mkPrimitive "writeArray#" $ return $ \(HeapArg ptr) (IntArg idx) (AnyArg val) RealWorld ->
      do Array arr <- fetch ptr
         let (before,after) = splitAt idx arr
         updateValue ptr (Array (before ++ [val] ++ drop 1 after))
         return realWorld

-- |Create @MutVar\#@ with specified initial value in specified state thread.
newMutVar
    = mkPrimitive "newMutVar#" $
      do tNode <- getUnboxedT
         return $ \(AnyArg val) RealWorld ->
                    do ptr <- storeValue val
                       return $ mkNode tNode [realWorld, HeapPointer ptr]

-- |Write contents of @MutVar\#@.
writeMutVar
    = mkPrimitive "writeMutVar#" $ return $ \(HeapArg ptr) (AnyArg val) RealWorld ->
      do updateValue ptr val
         return realWorld

-- |Read contents of @MutVar\#@. Result is not yet evaluated.
readMutVar
    = mkPrimitive "readMutVar#" $
      do ioNode <- getUnboxedT
         return $ \(HeapArg ptr) RealWorld ->
                  do val <- fetch ptr
                     return (mkNode ioNode [realWorld, val])

newMVar
    = mkPrimitive "newMVar#" $
      do tNode <- getUnboxedT
         return $ \RealWorld ->
                    do ptr <- storeValue Empty
                       return $ mkNode tNode [realWorld, (HeapPointer ptr)]
putMVar
    = mkPrimitive "putMVar#" $ return $ \(HeapArg ptr) (AnyArg val) RealWorld ->
      do updateValue ptr val
         return realWorld

takeMVar
    = mkPrimitive "takeMVar#" $
      do tNode <- getUnboxedT
         return $ \(HeapArg ptr) RealWorld ->
                    do val <- fetch ptr
                       return $ mkNode tNode [realWorld, val]


-- Dummy primitive
mkWeak = mkPrimitive "mkWeak#" $
         do tNode <- getUnboxedT
            return $ \(AnyArg key) (AnyArg val) (AnyArg finalizer) RealWorld ->
                       noScope $ return (mkNode tNode [realWorld, Empty])


narrow32Int
    = mkPrimitive "narrow32Int#" $ return $ \(IntArg i) -> noScope $ return (fromInt i)

narrow8Word
    = mkPrimitive "narrow8Word#" $ return $ \(IntArg i) -> noScope $ return (fromInt i)

int2Word
    = mkPrimitive "int2Word#" $ return $ \(IntArg i) -> noScope $ return (fromInt i)

word2Int
    = mkPrimitive "word2Int#" $ return $ \(IntArg i) -> noScope $ return (fromInt i)

negateInt
    = mkPrimitive "negateInt#" $ return $ \(IntArg i) -> noScope $ return (fromInt (negate i))





-- Primitive helpers


getUnboxedT
    = lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")

getUnboxedT0
    = lookupNode (fromString "ghc-prim:GHC.Prim.(# #)")

mkNode :: Renamed -> [EvalValue] -> EvalValue
mkNode node args
    = CNode node 0 args

noScope :: IO EvalValue -> CompValue
noScope fn = liftIO fn


runEvalPrimitive :: EvalValue -> CompValue
runEvalPrimitive (HeapPointer ptr)
    = worker =<< fetch ptr
    where worker orig@(FNode name fn 0 args)
              = do --liftIO $ putStrLn $ "Running: " ++ show name ++ " " ++ show args
                   reduced <- fn args
                   updateValue ptr reduced
                   return reduced
          worker val = return val
runEvalPrimitive val@FNode{} = return val
runEvalPrimitive val@Lit{} = return val
runEvalPrimitive val@CNode{} = return val
runEvalPrimitive Empty = return Empty
runEvalPrimitive val = error $ "unhandled eval: " ++ show val

fromPointer :: Ptr a -> EvalValue
fromPointer ptr = Lit (Lint $ fromIntegral (minusPtr ptr nullPtr))

fromInt :: Int -> EvalValue
fromInt i = Lit (Lint (fromIntegral i))

trueNode :: Gen Renamed
trueNode = lookupNode (fromString "ghc-prim:GHC.Bool.True")

falseNode :: Gen Renamed
falseNode = lookupNode (fromString "ghc-prim:GHC.Bool.False")

binOp :: (EvalValue -> EvalValue -> Bool) -> Gen (AnyArg -> AnyArg -> CompValue)
binOp fn
    = do t <- trueNode
         f <- falseNode
         return $ \(AnyArg a) (AnyArg b) ->
                     noScope $ if a `fn` b then return (mkNode t []) else return (mkNode f [])


binIntOp :: (Int -> Int -> Int) -> Gen (IntArg -> IntArg -> CompValue)
binIntOp fn
    = return $ \(IntArg a) (IntArg b) -> noScope $ return (Lit (Lint $ fromIntegral (fn a b)))

realWorld :: EvalValue
realWorld = Empty





-- Mechanism for the primitives

class IsPrimitive a where toPrimHandle :: String -> a -> [EvalValue] -> CompValue

instance (IsPrimitive b, FromArg a) => IsPrimitive (a -> b) where
    toPrimHandle name fn (x:xs) = do val <- liftIO $ fromArg x
                                     toPrimHandle name (fn val) xs
    toPrimHandle name fn [] = error $ "Grin.Eval.Primitives.toPrimHandle: Not enough arguments for: " ++ name

instance IsPrimitive (CompValue) where
    toPrimHandle name fn [] = fn
    toPrimHandle name fn _  = error $ "Grin.Eval.Primitives.toPrimHandle: Too many arguments to: " ++ name

class FromArg a where fromArg :: EvalValue -> IO a

instance FromArg AnyArg where
    fromArg = return . AnyArg
instance FromArg RealWorld where
    fromArg Empty = return RealWorld
    --fromArg (Node (Builtin name) (FunctionNode 0) []) | name == fromString "realWorld#" = return RealWorld
    fromArg (FNode _ _ 0 []) = return RealWorld
    fromArg v     = error $ "Grin.Eval.Primitives.fromArg: Expected realWorld: " ++ show v
instance FromArg IntArg where
    fromArg (Lit (Lint i)) = return (IntArg $ fromIntegral i)
    fromArg v              = error $ "Grin.Eval.Primitives.fromArg: Expected integer: " ++ show v
instance FromArg CharArg where
    fromArg (Lit (Lchar c)) = return (CharArg c)
    fromArg v               = error $ "Grin.Eval.Primitives.fromArg: Expected char: " ++ show v
instance FromArg ArrayArg where
    fromArg (Array arr) = return (ArrayArg arr)
    fromArg v           = error $ "Grin.Eval.Primitives.fromArg: Expected array: " ++ show v
instance FromArg HeapArg where
    fromArg (HeapPointer ptr) = return (HeapArg ptr)
    fromArg v                 = error $ "Grin.Eval.Primitives.fromArg: Expected heap pointer: " ++ show v
instance FromArg PtrArg where
    fromArg (Lit (Lstring str)) = do ptr <- liftIO $ newCString str
                                     return $ PtrArg (castPtr ptr)
    fromArg (Lit (Lint ptr))    = return $ PtrArg (nullPtr `plusPtr` fromIntegral ptr)
    fromArg v = error $ "Grin.Eval.Primitives.fromArg: Expected pointer: " ++ show v


mkPrimitive :: IsPrimitive a => String -> Gen a -> (String, GlobalScope -> Primitive)
mkPrimitive name fn
    = (name, \global -> Primitive { primName = name
                                  , primHandle = toPrimHandle name (fn global) })




