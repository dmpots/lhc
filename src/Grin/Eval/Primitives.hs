{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Grin.Eval.Primitives
    ( runPrimitive
    , runEvalPrimitive
    ) where

import CompactString
import Grin.Types hiding (Value(..))
import Grin.Eval.Types
import Grin.Eval.Basic ( callFunction, storeValue, updateValue, fetch, lookupNode )

import qualified Data.Map as Map
import Control.Monad.State
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C
import Foreign.Storable
import Data.Char; import Data.Word; import Data.Bits

newtype IntArg = IntArg Int
newtype CharArg = CharArg Char
newtype ArrayArg = ArrayArg [EvalValue]
newtype AnyArg = AnyArg EvalValue
newtype PtrArg = PtrArg (Ptr ())
newtype HeapArg = HeapArg HeapPointer
data RealWorld = RealWorld

data Primitive = Primitive { primName :: String, primHandle :: ([EvalValue] -> Eval EvalValue) }

-- Primitive handlers

runPrimitive :: CompactString -> [EvalValue] -> Eval EvalValue
runPrimitive name args
    = case Map.lookup name allPrimitives of
        Nothing   -> error $ "Grin.Eval.Primitives.runPrimitive: Unknown primitive: " ++ show name
        Just prim -> primHandle prim args

allPrimitives :: Map.Map CompactString Primitive
allPrimitives = Map.fromList [ (fromString (primName prim), prim) | prim <- prims ]
    where prims = [ equal, gt, lt, gte, lte
                  , plus, minus, times, remInt, quotInt, addIntC
                  , chrPrim
                  , indexCharOffAddr
                  , writeCharArray
                  , noDuplicate
                  , realWorldPrim, myThreadIdPrim
                  , catchPrim, blockAsyncExceptions, unblockAsyncExceptions
                  , newPinnedByteArray
                  , updatePrim, evalPrim, applyPrim
                  , newArray, readArray, writeArray
                  , newMutVar, writeMutVar, readMutVar
                  , narrow32Int, int2Word, negateInt
                  , newMVar, putMVar, takeMVar
                  , mkWeak]




-- Primitive definitions

equal, gt, lt :: Primitive
equal = mkPrimitive "==#" $ binOp (==)
gt    = mkPrimitive ">#" $ binOp (>)
lt    = mkPrimitive "<#" $ binOp (<)
gte   = mkPrimitive ">=#" $ binOp (>=)
lte   = mkPrimitive "<=#" $ binOp (<=)

plus :: Primitive
plus = mkPrimitive "+#" $ binIntOp (+)
minus :: Primitive
minus = mkPrimitive "-#" $ binIntOp (-)
times :: Primitive
times = mkPrimitive "*#" $ binIntOp (*)
remInt :: Primitive
remInt = mkPrimitive "remInt#" $ binIntOp rem
quotInt :: Primitive
quotInt = mkPrimitive "quotInt#" $ binIntOp quot
addIntC :: Primitive
addIntC = mkPrimitive "addIntC#" $ \(IntArg a) (IntArg b) ->
          let c = fromIntegral a + fromIntegral b
              o = c `shiftR` bitSize (0::Int)
          in returnT (Lit (Lint c)) (Lit (Lint o))


chrPrim :: Primitive
chrPrim
    = mkPrimitive "chr#" $ \(IntArg i) ->
      return $ Lit $ Lchar (chr i) :: Eval EvalValue

-- |Reads 8-bit character; offset in bytes.
indexCharOffAddr :: Primitive
indexCharOffAddr
    = mkPrimitive "indexCharOffAddr#" $ \(PtrArg ptr) (IntArg nth) ->
      do c <- liftIO $ peekByteOff ptr nth :: Eval Word8
         return (Lit (Lchar (chr (fromIntegral c))))

-- |Write 8-bit character; offset in bytes.
writeCharArray :: Primitive
writeCharArray
    = mkPrimitive "writeCharArray#" $ \(PtrArg ptr) (IntArg offset) (CharArg c) RealWorld ->
      do liftIO $ poke (ptr `plusPtr` offset) (fromIntegral (ord c) :: Word8)
         return realWorld :: Eval EvalValue


noDuplicate :: Primitive
noDuplicate = mkPrimitive "noDuplicate#" $ \RealWorld -> return realWorld :: Eval EvalValue

realWorldPrim :: Primitive
realWorldPrim = mkPrimitive "realWorld#" $ (return realWorld :: Eval EvalValue)

myThreadIdPrim :: Primitive
myThreadIdPrim
    = mkPrimitive "myThreadId#" $ \RealWorld ->
      returnIO (Lit (Lint 0))

catchPrim :: Primitive
catchPrim
    = mkPrimitive "catch#" $ \(AnyArg fn) (AnyArg handler) RealWorld ->
      callFunction (Builtin $ fromString "apply") [fn, realWorld]

blockAsyncExceptions :: Primitive
blockAsyncExceptions
    = mkPrimitive "blockAsyncExceptions#" $ \(AnyArg fn) RealWorld ->
      callFunction (Builtin $ fromString "apply") [fn, realWorld]

unblockAsyncExceptions :: Primitive
unblockAsyncExceptions
    = mkPrimitive "unblockAsyncExceptions#" $ \(AnyArg fn) RealWorld ->
      callFunction (Builtin $ fromString "apply") [fn, realWorld]

-- |Create a mutable byte array that the GC guarantees not to move.
newPinnedByteArray :: Primitive
newPinnedByteArray
    = mkPrimitive "newPinnedByteArray#" $ \(IntArg size) RealWorld ->
      do ptr <- liftIO $ mallocBytes size
         returnIO (fromPointer ptr)

updatePrim :: Primitive
updatePrim
    = mkPrimitive "update" $ \(HeapArg ptr) (AnyArg val) ->
      do updateValue ptr val
         return Empty

evalPrim :: Primitive
evalPrim
    = mkPrimitive "eval" $ \(AnyArg arg) ->
      runEvalPrimitive arg

applyPrim :: Primitive
applyPrim
    = mkPrimitive "apply" $ \(AnyArg fnPtr) (AnyArg arg) ->
      do fn <- runEvalPrimitive fnPtr
         case fn of
           Node nodeName (FunctionNode 1) args -> callFunction nodeName (args ++ [arg])
           Node nodeName (FunctionNode 0) args -> error $ "apply: over application?"
           Node nodeName (FunctionNode n) args -> return $ Node nodeName (FunctionNode (n-1)) (args ++ [arg])
           _ -> error $ "weird apply: " ++ (show fn)


newArray :: Primitive
newArray
    = mkPrimitive "newArray#" $ \(IntArg len) (AnyArg elt) RealWorld ->
      do ptr <- storeValue (Array $ replicate len elt)
         returnIO (HeapPointer ptr)

readArray :: Primitive
readArray
    = mkPrimitive "readArray#" $ \(HeapArg ptr) (IntArg idx) RealWorld ->
      do Array arr <- fetch ptr
         returnIO $ arr!!idx

writeArray :: Primitive
writeArray
    = mkPrimitive "writeArray#" $ \(HeapArg ptr) (IntArg idx) (AnyArg val) RealWorld ->
      do Array arr <- fetch ptr
         let (before,after) = splitAt idx arr
         updateValue ptr (Array (before ++ [val] ++ drop 1 after))
         return realWorld

newMutVar, writeMutVar, readMutVar :: Primitive
-- |Create @MutVar\#@ with specified initial value in specified state thread.
newMutVar
    = mkPrimitive "newMutVar#" $ \(AnyArg val) RealWorld ->
      do ptr <- storeValue val
         returnIO (HeapPointer ptr)

-- |Write contents of @MutVar\#@.
writeMutVar
    = mkPrimitive "writeMutVar#" $ \(HeapArg ptr) (AnyArg val) RealWorld ->
      do updateValue ptr val
         return realWorld

-- |Read contents of @MutVar\#@. Result is not yet evaluated.
readMutVar
    = mkPrimitive "readMutVar#" $ \(AnyArg ptr) RealWorld ->
      do returnIO ptr


newMVar, putMVar, takeMVar :: Primitive
newMVar
    = mkPrimitive "newMVar#" $ \RealWorld ->
      do ptr <- storeValue Empty
         returnIO (HeapPointer ptr)
putMVar
    = mkPrimitive "putMVar#" $ \(HeapArg ptr) (AnyArg val) RealWorld ->
      do updateValue ptr val
         return realWorld

takeMVar
    = mkPrimitive "takeMVar#" $ \(HeapArg ptr) RealWorld ->
      do val <- fetch ptr
         returnIO val


-- Dummy primitive
mkWeak :: Primitive
mkWeak = mkPrimitive "mkWeak#" $ \(AnyArg key) (AnyArg val) (AnyArg finalizer) RealWorld ->
         do returnIO Empty


narrow32Int :: Primitive
narrow32Int
    = mkPrimitive "narrow32Int#" $ \(IntArg i) -> return (fromInt i) :: Eval EvalValue

int2Word :: Primitive
int2Word
    = mkPrimitive "int2Word#" $ \(IntArg i) -> return (fromInt i) :: Eval EvalValue

negateInt :: Primitive
negateInt
    = mkPrimitive "negateInt#" $ \(IntArg i) -> return (fromInt (negate i)) :: Eval EvalValue





-- Primitive helpers

runEvalPrimitive :: EvalValue -> Eval EvalValue
runEvalPrimitive (HeapPointer ptr) = do reduced <- runEvalPrimitive =<< fetch ptr
                                        updateValue ptr reduced
                                        return reduced
runEvalPrimitive (Node fn (FunctionNode 0) args) = callFunction fn args
runEvalPrimitive val = return val


fromPointer :: Ptr a -> EvalValue
fromPointer ptr = Lit (Lint $ fromIntegral (minusPtr ptr nullPtr))

fromInt :: Int -> EvalValue
fromInt i = Lit (Lint (fromIntegral i))

returnT :: EvalValue -> EvalValue -> Eval EvalValue
returnT a b
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [a, b]

returnIO :: EvalValue -> Eval EvalValue
returnIO  = returnT realWorld

trueNode :: Eval EvalValue
trueNode
    = do node <- lookupNode (fromString "ghc-prim:GHC.Bool.True")
         return $ Node node (ConstructorNode 0) []

falseNode :: Eval EvalValue
falseNode
    = do node <- lookupNode (fromString "ghc-prim:GHC.Bool.False")
         return $ Node node (ConstructorNode 0) []

binOp fn (AnyArg a) (AnyArg b)
    = if a `fn` b
      then trueNode
      else falseNode

binIntOp :: (Int -> Int -> Int) -> IntArg -> IntArg -> Eval EvalValue
binIntOp fn (IntArg a) (IntArg b)
    = return (Lit (Lint $ fromIntegral (fn a b)))

realWorld :: EvalValue
realWorld = Empty





-- Mechanism for the primitives

class IsPrimitive a where toPrimHandle :: String -> a -> [EvalValue] -> Eval EvalValue

instance (IsPrimitive b, FromArg a) => IsPrimitive (a -> b) where
    toPrimHandle name fn (x:xs) = do val <- fromArg x
                                     toPrimHandle name (fn val) xs
    toPrimHandle name fn [] = error $ "Grin.Eval.Primitives.toPrimHandle: Not enough arguments for: " ++ name

instance IsPrimitive (Eval EvalValue) where
    toPrimHandle name fn [] = fn
    toPrimHandle name fn _  = error $ "Grin.Eval.Primitives.toPrimHandle: Too many arguments to: " ++ name

class FromArg a where fromArg :: EvalValue -> Eval a

instance FromArg AnyArg where
    fromArg = return . AnyArg
instance FromArg RealWorld where
    fromArg Empty = return RealWorld
    fromArg (Node (Builtin name) (FunctionNode 0) []) | name == fromString "realWorld#" = return RealWorld
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


mkPrimitive :: IsPrimitive a => String -> a -> Primitive
mkPrimitive name fn
    = Primitive { primName = name
                , primHandle = toPrimHandle name fn }




