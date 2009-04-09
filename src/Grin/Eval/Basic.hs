module Grin.Eval.Basic where

import CompactString
import Grin.Types hiding (Value(..))
import qualified Grin.Types as Grin
import Grin.Pretty

import Control.Monad (ap)
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable
import Data.Char
import Data.Word
import Foreign.C.String
import System.Posix (Fd(..), fdWrite)

import Debug.Trace

eval :: Grin -> String -> IO EvalValue
eval grin entry
    = runEval grin $
      do ptr <- storeValue =<< runEvalPrimitive =<< lookupVariable renamedEntry
         callFunction (Builtin $ fromString "apply") [HeapPointer ptr, Empty]
    where renamedEntry = case [ renamed | CAF{cafName = renamed@(Aliased _ name)} <- grinCAFs grin, name == fromString entry ] of
                           []       -> error $ "Grin.Eval.Basic.evaluate: couldn't find entry point: " ++ entry
                           (name:_) -> name


type Scope = Map.Map Renamed EvalValue
type HeapPointer = Int
type Heap = Map.Map HeapPointer EvalValue
data EvalValue
    = Node Renamed NodeType [EvalValue]
    | Lit Lit
    | HeapPointer HeapPointer
    | Hole Int
    | Empty
      deriving (Show,Eq,Ord)
data EvalState
    = EvalState { stateFunctions :: Map.Map Renamed FuncDef
                , stateNodes     :: Map.Map CompactString NodeDef
                , stateHeap      :: Heap
                , stateFree      :: HeapPointer }
type Eval a = StateT EvalState (ReaderT Scope IO) a

runEval :: Grin -> Eval a -> IO a
runEval grin fn
    = runReaderT (evalStateT withCAFs initState) emptyScope
    where withCAFs = do values <- mapM (\v -> storeValue =<< toEvalValue v) (map cafValue (grinCAFs grin))
                        bindValues (zip (map cafName (grinCAFs grin)) (map HeapPointer values)) fn
          emptyScope = Map.empty
          initState = EvalState { stateFunctions = Map.fromList [ (funcDefName def, def) | def <- grinFunctions grin ]
                                , stateNodes     = Map.fromList [ (name, node) | node@NodeDef{nodeName = Aliased _ name} <- grinNodes grin ]
                                , stateHeap      = Map.empty
                                , stateFree      = 0 }

callFunction :: Renamed -> [EvalValue] -> Eval EvalValue
callFunction (Builtin fnName) [arg] | fnName == fromString "eval"
    = runEvalPrimitive arg
callFunction (Builtin fnName) [fnPtr,arg] | fnName == fromString "apply"
    = do fn <- runEvalPrimitive fnPtr
         case fn of
           Node nodeName (FunctionNode 1) args -> callFunction nodeName (args ++ [arg])
           Node nodeName (FunctionNode 0) args -> error $ "apply: over application?"
           Node nodeName (FunctionNode n) args -> return $ Node nodeName (FunctionNode (n-1)) (args ++ [arg])
           _ -> error $ "apply: " ++ (show fn)
callFunction (Builtin fnName) [fn, handler, realWorld] | fnName == fromString "catch#"
    = callFunction (Builtin $ fromString "apply") [fn, realWorld]
callFunction (Builtin fnName) [fn, realWorld] | fnName == fromString "blockAsyncExceptions#"
    = callFunction (Builtin $ fromString "apply") [fn, realWorld]
callFunction (Builtin fnName) [realWorld] | fnName == fromString "noDuplicate#"
    = return $ realWorld
callFunction (Builtin fnName) [] | fnName == fromString "realWorld#"
    = return Empty
callFunction (Builtin fnName) [int] | fnName == fromString "int2Word#"
    = return int
callFunction (Builtin fnName) [addr,Lit (Lint nth)] | fnName == fromString "indexCharOffAddr#"
    = do case addr of
           Lit (Lstring str) -> return (Lit (Lchar ((str++"\x0")!!fromIntegral nth)))
           _ -> error $ "indexCharOffAddr#: " ++ show addr
callFunction (Builtin fnName) [a,b] | fnName == fromString "==#"
    = do true <- lookupNode (fromString "ghc-prim:GHC.Bool.True")
         false <- lookupNode (fromString "ghc-prim:GHC.Bool.False")
         if a == b
            then return $ Node true (ConstructorNode 0) []
            else return $ Node false (ConstructorNode 0) []
callFunction (Builtin fnName) [a,b] | fnName == fromString ">#"
    = do true <- lookupNode (fromString "ghc-prim:GHC.Bool.True")
         false <- lookupNode (fromString "ghc-prim:GHC.Bool.False")
         if a > b
            then return $ Node true (ConstructorNode 0) []
            else return $ Node false (ConstructorNode 0) []
callFunction (Builtin fnName) [a,b] | fnName == fromString "<#"
    = do true <- lookupNode (fromString "ghc-prim:GHC.Bool.True")
         false <- lookupNode (fromString "ghc-prim:GHC.Bool.False")
         if a > b
            then return $ Node true (ConstructorNode 0) []
            else return $ Node false (ConstructorNode 0) []
callFunction (Builtin fnName) [a,b] | fnName == fromString ">=#"
    = do true <- lookupNode (fromString "ghc-prim:GHC.Bool.True")
         false <- lookupNode (fromString "ghc-prim:GHC.Bool.False")
         if a >= b
            then return $ Node true (ConstructorNode 0) []
            else return $ Node false (ConstructorNode 0) []
callFunction (Builtin fnName) [a,b] | fnName == fromString "<=#"
    = do true <- lookupNode (fromString "ghc-prim:GHC.Bool.True")
         false <- lookupNode (fromString "ghc-prim:GHC.Bool.False")
         if a <= b
            then return $ Node true (ConstructorNode 0) []
            else return $ Node false (ConstructorNode 0) []
callFunction (Builtin fnName) [Lit (Lint a)] | fnName == fromString "chr#"
    = return $ Lit (Lchar $ chr $ fromIntegral a)
callFunction (Builtin fnName) [Lit (Lint a)] | fnName == fromString "negateInt#"
    = return $ Lit (Lint $ negate a)
callFunction (Builtin fnName) [Lit (Lint a),Lit (Lint b)] | fnName == fromString "+#"
    = return $ Lit (Lint (a+b))
callFunction (Builtin fnName) [Lit (Lint a),Lit (Lint b)] | fnName == fromString "-#"
    = return $ Lit (Lint (a-b))
callFunction (Builtin fnName) [Lit (Lint a),Lit (Lint b)] | fnName == fromString "remInt#"
    = do --liftIO $ putStrLn $ "rem: " ++ show (a,b)
         return $ Lit (Lint (a `rem` b))
callFunction (Builtin fnName) [Lit (Lint a),Lit (Lint b)] | fnName == fromString "quotInt#"
    = do return $ Lit (Lint (a `quot` b))
callFunction (Builtin fnName) [a,b] | fnName == fromString "-#"
    = do Lit (Lint a') <- runEvalPrimitive a
         Lit (Lint b') <- runEvalPrimitive b
         return (Lit (Lint (a'-b')))
callFunction (Builtin fnName) [a,b] | fnName == fromString "+#"
    = do Lit (Lint a') <- runEvalPrimitive a
         Lit (Lint b') <- runEvalPrimitive b
         return (Lit (Lint (a'+b')))
callFunction (Builtin fnName) [Lit (Lint ptr), Lit (Lint offset),Lit (Lchar c),realWorld] | fnName == fromString "writeCharArray#"
    = do liftIO $ poke (nullPtr `plusPtr` (fromIntegral $ ptr + offset)) (fromIntegral (ord c) :: Word8)
         --liftIO $ putStrLn $ "writeCharArray: " ++ show c
         return realWorld
callFunction (Builtin fnName) [Lit (Lint size), realWorld] | fnName == fromString "newPinnedByteArray#"
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         ptr <- liftIO $ mallocBytes (fromIntegral size)
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint $ fromIntegral $ minusPtr ptr nullPtr)]
callFunction (Builtin fnName) [val, realWorld] | fnName == fromString "newMutVar#"
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         ptr <- storeValue val
         return $ Node node (ConstructorNode 0) [realWorld, HeapPointer ptr]
callFunction (Builtin fnName) [ptr, realWorld] | fnName == fromString "readMutVar#"
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, ptr]
callFunction (Builtin fnName) [ptr, val,realWorld] | fnName == fromString "writeMutVar#"
    = do --liftIO $ putStrLn $ "writeMutVar: " ++ show (ptr,val)
         HeapPointer hpPtr <- return ptr
         updateValue hpPtr val
         return $ realWorld
callFunction (Builtin fnName) [realWorld] | fnName == fromString "newMVar#"
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         ptr <- storeValue Empty
         --trace ("new mvar at: " ++ show ptr) $ return ()
         return $ Node node (ConstructorNode 0) [realWorld, HeapPointer ptr]
callFunction (Builtin fnName) [mvar,realWorld] | fnName == fromString "takeMVar#"
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         HeapPointer ptr <- return mvar
         val <- fetch ptr
         --trace ("Reading mvar: " ++ show (ptr,val)) $ return ()
         return $ Node node (ConstructorNode 0) [realWorld, val]
callFunction (Builtin fnName) [mvar,val,realWorld] | fnName == fromString "putMVar#"
    = do HeapPointer ptr <- return mvar
         updateValue ptr val
         --trace ("updating mvar: " ++ show (ptr,val)) $ return ()
         return $ realWorld
callFunction (Builtin fnName) [mvar,val] | fnName == fromString "update"
    = do HeapPointer ptr <- return mvar
         updateValue ptr val
         --trace ("updating var: " ++ show (ptr,val)) $ return ()
         return $ Empty
callFunction (Builtin fnName) [intVal] | fnName == fromString "narrow32Int#"
    = do Lit (Lint val) <- runEvalPrimitive intVal
         return $ Lit (Lint val)
callFunction (Builtin fnName) [exp,realWorld] | fnName == fromString "raiseIO#"
    = do val <- runEvalPrimitive exp
         error $ "RaiseIO: " ++ show val
callFunction (Builtin fnName) [key,val,finalizer,realWorld] | fnName == fromString "mkWeak#"
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Empty]
callFunction (External "__hscore_memcpy_dst_off") [Lit (Lint dst),Lit (Lint off),Lit (Lint src),Lit (Lint size), realWorld]
    = do let dstPtr = nullPtr `plusPtr` (fromIntegral (dst+off))
             srcPtr = nullPtr `plusPtr` (fromIntegral src)
         liftIO $ copyBytes dstPtr srcPtr (fromIntegral size)
         node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint (dst+off))]
callFunction (External "__hscore_PrelHandle_write") [Lit (Lint fd),Lit (Lint ptr),Lit (Lint offset),Lit (Lint size),realWorld]
    = do --liftIO $ putStrLn $ "Writing to: " ++ show (fd,size)
         let strPtr = nullPtr `plusPtr` fromIntegral (ptr+offset)
         str <- liftIO $ peekCStringLen (strPtr,fromIntegral size)
         out <- liftIO $ fdWrite (Fd (fromIntegral fd)) str
         node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint $ fromIntegral out)]
callFunction (External "__hscore_get_errno") [realWorld]
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint 0)]
callFunction (External "__hscore_bufsiz") [realWorld]
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint 512)]
callFunction (External "isatty") [fd,realWorld]
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint 1)]
callFunction (External "fdReady") [fd,write,msecs,isSock,realWorld]
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint 1)]
callFunction (External "rtsSupportsBoundThreads") [realWorld]
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint 1)]
callFunction fnName args
    = do fn <- lookupFunction fnName
         --liftIO $ putStrLn $ "Entering: " ++ show fnName
         ret <- runFunction fn args
         --liftIO $ putStrLn $ "Returning: " ++ show fnName ++ ", value: " ++ show ret
         return ret

runFunction :: FuncDef -> [EvalValue] -> Eval EvalValue
runFunction def args
    = case length args `compare` length (funcDefArgs def) of
        LT -> error $ "Grin.Eval.Basic.runFunction: Too few arguments for: " ++ show (funcDefName def)
        GT -> error $ "Grin.Eval.Basic.runFunction: Too many arguments for: " ++ show (funcDefName def)
        EQ -> bindValues (zip (funcDefArgs def) args) $
              runExpression (funcDefBody def)

runExpression :: Expression -> Eval EvalValue
runExpression (Unit v) = toEvalValue v
runExpression (e :>>= bind :-> lam)
    = do e' <- runExpression e
         bindLambda e' bind $ runExpression lam
runExpression (Store v)
    = do ptr <- storeValue =<< toEvalValue v
         return $ HeapPointer ptr
runExpression (Application fn args)
    = callFunction fn =<< mapM toEvalValue args
runExpression (Case val alts)
    = do val' <- {- trace ("Case for: " ++ show (ppValue val)) $ -} toEvalValue val
         runCase val' alts
runExpression e = error $ "Unhandled expression: " ++ (show (ppExpression e))

runCase :: EvalValue -> [Lambda] -> Eval EvalValue
runCase val@(Node nodeName _type args) alts
    = worker alts
    where worker [] = error $ "Grin.Eval.Basic.runCase: no match found: " ++ show (val)
          worker [Grin.Variable x :-> e] = bindValue x val (runExpression e)
          worker ((Grin.Variable x :-> e):y:ys) = worker (y:(Grin.Variable x :-> e):ys)
          worker ((Grin.Node tag _type tagArgs :-> e):_) | tag == nodeName
              = bindLambdas (zip args tagArgs) (runExpression e)
          worker (x:xs)
              = worker xs
runCase (Lit lit) alts
    = worker alts
    where worker [] = error $ "no match found."
          worker [Grin.Variable x :-> e] = bindValue x (Lit lit) (runExpression e)
          worker ((Grin.Variable x :-> e):y:ys) = worker (y:(Grin.Variable x :-> e):ys)
          worker ((Grin.Lit lit' :-> e):_) | lit' == lit
              = runExpression e
          worker (x:xs) = worker xs
runCase val [Grin.Variable x :-> e]
    = bindValue x val (runExpression e)
runCase val alts = error $ "runCase: " ++ (show (val,length alts))



runEvalPrimitive (HeapPointer ptr) = do reduced <- runEvalPrimitive =<< fetch ptr
                                        updateValue ptr reduced
                                        return reduced
runEvalPrimitive (Node fn (FunctionNode 0) args) = callFunction fn args
runEvalPrimitive val = return val

storeValue :: EvalValue -> Eval HeapPointer
storeValue val
    = do st <- get
         let newFree = stateFree st + 1
         put st{stateFree = newFree
               ,stateHeap = Map.insert (stateFree st) val (stateHeap st)}
         {- trace ("Storing value at: " ++ show (stateFree st, val)) $ -}
         return (stateFree st)

updateValue :: HeapPointer -> EvalValue -> Eval ()
updateValue ptr val
    = modify $ \st -> st{stateHeap = Map.alter fn ptr (stateHeap st)}
    where fn _ = Just val

bindLambda :: EvalValue -> Grin.Value -> Eval a -> Eval a
bindLambda (Node tag _ args) (Grin.Node bTag _ bArgs) | length args == length bArgs
    = bindLambdas (zip args bArgs)
bindLambda from (Grin.Variable to)
    = bindValue to from
bindLambda from Grin.Empty
    = id
bindLambda from to
    = error $ "bindLambda: " ++ (show (from, to))

bindLambdas :: [(EvalValue, Grin.Value)] -> Eval a -> Eval a
bindLambdas [] = id
bindLambdas ((a,b):xs) = bindLambda a b . bindLambdas xs


bindValue :: Renamed -> EvalValue -> Eval a -> Eval a
bindValue variable value
    = local (Map.insert variable value)

bindValues :: [(Renamed, EvalValue)] -> Eval a -> Eval a
bindValues [] = id
bindValues ((variable,value):xs) = bindValue variable value . bindValues xs

lookupFunction :: Renamed -> Eval FuncDef
lookupFunction name = gets $ \st -> Map.findWithDefault errMsg name (stateFunctions st)
    where errMsg = error $ "Grin.Eval.Basic.lookupFunction: couldn't find function: " ++ show name

lookupVariable :: Renamed -> Eval EvalValue
lookupVariable variable
    = asks $ Map.findWithDefault errMsg variable
    where errMsg = error $ "Grin.Eval.Basic.lookupVariable: couldn't find variable: " ++ show variable

lookupNode :: CompactString -> Eval Renamed
lookupNode name
    = gets $ \st -> nodeName $ Map.findWithDefault errMsg name (stateNodes st)
    where errMsg = error $ "Couldn't find node: " ++ show name

fetch :: HeapPointer -> Eval EvalValue
fetch ptr
    = gets $ \st -> Map.findWithDefault errMsg ptr (stateHeap st)
    where errMsg = error $ "Grin.Eval.Basic.fetch: couldn't find heap value for: " ++ show ptr

toEvalValue :: Grin.Value -> Eval EvalValue
toEvalValue (Grin.Node name ty args) = return (Node name ty) `ap` (mapM toEvalValue args)
toEvalValue (Grin.Lit lit)           = return $ Lit lit
toEvalValue (Grin.Variable var)      = lookupVariable var
toEvalValue (Grin.Hole size)         = return $ Hole size
toEvalValue (Grin.Empty)             = return Empty
