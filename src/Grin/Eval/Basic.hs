module Grin.Eval.Basic
    ( eval
    , EvalValue(..)
    , callFunction
    , storeValue
    , updateValue
    , fetch
    , lookupNode
    ) where

import CompactString
import Grin.Types hiding (Value(..))
import qualified Grin.Types as Grin
import Grin.Pretty
import Grin.Eval.Types
import {-# SOURCE #-} Grin.Eval.Primitives

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Foreign.Ptr
import Foreign.Marshal
import Data.Char
import Foreign.C
import Foreign.C.String
import Foreign.Storable
import System.Posix (Fd(..), fdWrite)

import Foreign.LibFFI
import System.Posix.DynamicLinker

eval :: Grin -> String -> [String] -> IO EvalValue
eval grin entry commandArgs
    = runEval grin $
      do setCommandArgs ("lhc":commandArgs)
         ptr <- storeValue =<< runEvalPrimitive =<< lookupVariable renamedEntry
         callFunction (Builtin $ fromString "apply") [HeapPointer ptr, Empty]
    where renamedEntry = case [ renamed | CAF{cafName = renamed@(Aliased _ name)} <- grinCAFs grin, name == fromString entry ] of
                           []       -> error $ "Grin.Eval.Basic.evaluate: couldn't find entry point: " ++ entry
                           (name:_) -> name



runEval :: Grin -> Eval a -> IO a
runEval grin fn
    = runReaderT (evalStateT (unEval withCAFs) initState) emptyScope
    where withCAFs = do values <- mapM (\v -> storeValue =<< toEvalValue v) (map cafValue (grinCAFs grin))
                        bindCafValues (zip (map cafName (grinCAFs grin)) (map HeapPointer values)) fn
          emptyScope = Scope Map.empty Map.empty
          initState = EvalState { stateFunctions = Map.fromList [ (funcDefName def, def) | def <- grinFunctions grin ]
                                , stateNodes     = Map.fromList [ (name, node) | node@NodeDef{nodeName = Aliased _ name} <- grinNodes grin ]
                                , stateHeap      = Map.empty
                                , stateFree      = 0
                                , stateArgs      = ["lhc"] }

callFunction :: Renamed -> [EvalValue] -> Eval EvalValue
callFunction (Builtin fnName) args = runPrimitive fnName args

-- These functions are defined in the base library. I'm not sure how to deal with this properly.
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
callFunction (External "fdReady") [fd,write,msecs,isSock,realWorld]
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint 1)]
callFunction (External "rtsSupportsBoundThreads") [realWorld]
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint 1)]
callFunction (External "stg_sig_install") [signo, actioncode, ptr, realWorld]
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         return $ Node node (ConstructorNode 0) [realWorld, Lit (Lint 0)]
callFunction (External "getProgArgv") [Lit (Lint argcPtr), Lit (Lint argvPtr), realWorld]
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(# #)")
         args <- getCommandArgs
         liftIO $ poke (nullPtr `plusPtr` fromIntegral argcPtr) (fromIntegral (length args) :: CInt)
         cs <- liftIO $ newArray =<< mapM newCString args
         liftIO $ poke (nullPtr `plusPtr` fromIntegral argvPtr) cs
         return $ Node node (ConstructorNode 0) [realWorld]

-- If we don't recognize the function, try loading it through the linker.
callFunction (External name) args
    = do node <- lookupNode (fromString "ghc-prim:GHC.Prim.(#,#)")
         fnPtr <- liftIO $ dlsym Default name
         let toCArg (Lit (Lint i)) = argCInt (fromIntegral i)
         ret <- liftIO $ callFFI fnPtr retCInt (map toCArg $ init args)
         return $ Node node (ConstructorNode 0) [last args, Lit (Lint (fromIntegral ret))]

-- It if isn't a Builtin or External then it must be a local GRIN function. Call it!
callFunction fnName args
    = do fn <- lookupFunction fnName
         --liftIO $ putStrLn $ "Entering: " ++ show fnName ++ " " ++ unwords (map show args)
         ret <- clearLocalScope $ runFunction fn args
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
    = do val' <- toEvalValue val
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


storeValue :: EvalValue -> Eval HeapPointer
storeValue val
    = do st <- get
         let newFree = stateFree st + 1
         put st{stateFree = newFree
               ,stateHeap = Map.insert (stateFree st) val (stateHeap st)}
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
    = local (\scope -> scope{localScope = Map.insert variable value (localScope scope)})

bindCafValue :: Renamed -> EvalValue -> Eval a -> Eval a
bindCafValue variable value
    = local (\scope -> scope{globalScope = Map.insert variable value (globalScope scope)})

bindValues :: [(Renamed, EvalValue)] -> Eval a -> Eval a
bindValues [] = id
bindValues ((variable,value):xs) = bindValue variable value . bindValues xs

bindCafValues :: [(Renamed, EvalValue)] -> Eval a -> Eval a
bindCafValues [] = id
bindCafValues ((variable,value):xs) = bindCafValue variable value . bindCafValues xs

clearLocalScope :: Eval a -> Eval a
clearLocalScope = local $ \scope -> scope{localScope = Map.empty}

lookupFunction :: Renamed -> Eval FuncDef
lookupFunction name = gets $ \st -> Map.findWithDefault errMsg name (stateFunctions st)
    where errMsg = error $ "Grin.Eval.Basic.lookupFunction: couldn't find function: " ++ show name

lookupVariable :: Renamed -> Eval EvalValue
lookupVariable variable
    = asks $ \scope -> case Map.lookup variable (localScope scope) `mplus` Map.lookup variable (globalScope scope) of
                         Nothing -> errMsg
                         Just v  -> v
    where errMsg = error $ "Grin.Eval.Basic.lookupVariable: couldn't find variable: " ++ show variable

lookupNode :: CompactString -> Eval Renamed
lookupNode name
    = gets $ \st -> nodeName $ Map.findWithDefault errMsg name (stateNodes st)
    where errMsg = error $ "Couldn't find node: " ++ show name

fetch :: HeapPointer -> Eval EvalValue
fetch ptr
    = gets $ \st -> Map.findWithDefault errMsg ptr (stateHeap st)
    where errMsg = error $ "Grin.Eval.Basic.fetch: couldn't find heap value for: " ++ show ptr

setCommandArgs :: [String] -> Eval ()
setCommandArgs args = modify $ \st -> st{stateArgs = args}

getCommandArgs :: Eval [String]
getCommandArgs = gets stateArgs

toEvalValue :: Grin.Value -> Eval EvalValue
toEvalValue (Grin.Node name ty args) = return (Node name ty) `ap` (mapM toEvalValue args)
toEvalValue (Grin.Lit lit)           = return $ Lit lit
toEvalValue (Grin.Variable var)      = lookupVariable var
toEvalValue (Grin.Hole size)         = return $ Hole size
toEvalValue (Grin.Empty)             = return Empty
