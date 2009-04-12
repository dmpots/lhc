module Grin.Eval.Methods where

import CompactString
import Grin.Types hiding (Value)
import qualified Grin.Types as Grin
import Grin.Eval.Types

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader


setCommandArgs :: [String] -> Comp ()
setCommandArgs args = modify $ \st -> st{stateArgs = args}

getCommandArgs :: Comp [String]
getCommandArgs = gets stateArgs


lookupFunction :: Renamed -> Gen CompFunction
lookupFunction name globalScope
    = case Map.lookup name (globalFuncs globalScope) of
        Just fn -> fn
        Nothing -> error $ "Grin.Eval.Compile.lookupFunction: Couldn't find function: " ++ show name

fetch :: HeapPointer -> CompValue
fetch ptr
    = gets $ \st -> Map.findWithDefault errMsg ptr (stateHeap st)
    where errMsg = error $ "Grin.Eval.Compile.fetch: couldn't find heap value for: " ++ show ptr

storeValue :: EvalValue -> Comp HeapPointer
storeValue val
    = do st <- get
         let newFree = stateFree st + 1
         put st{stateFree = newFree
                  ,stateHeap = Map.insert (stateFree st) val (stateHeap st)}
         return (stateFree st)

updateValue :: HeapPointer -> EvalValue -> Comp ()
updateValue ptr val
    = modify $ \st -> st{stateHeap = Map.alter fn ptr (stateHeap st)}
    where fn _ = Just val


lookupVariable :: Renamed -> Gen CompValue
lookupVariable var globalScope
    = case Map.lookup var (globalCAFs globalScope) of
        Just caf -> return $ HeapPointer caf
        Nothing  -> asks (Map.findWithDefault errMsg var)
    where errMsg = error $ "Grin.Compile.RunTime.lookupVariable: couldn't find variable: " ++ show var

lookupNode :: CompactString -> Gen Renamed
lookupNode name globalScope
    = case Map.lookup name (globalNodes globalScope) of
        Just node -> nodeName node
        Nothing   -> error $ "Grin.Eval.Compile.lookupNode: couldn't find node: " ++ show name

