{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Grin.Eval.Types where

import CompactString
import Grin.Types

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader


type Scope = Map.Map Renamed EvalValue
type HeapPointer = Int
type Heap = Map.Map HeapPointer EvalValue
data EvalValue
    = Node Renamed NodeType [EvalValue]
    | Lit Lit
    | HeapPointer HeapPointer
    | Hole Int
    | Array [EvalValue]
    | Empty
      deriving (Show,Eq,Ord)
data EvalState
    = EvalState { stateFunctions :: Map.Map Renamed FuncDef
                , stateNodes     :: Map.Map CompactString NodeDef
                , stateHeap      :: Heap
                , stateFree      :: HeapPointer
                , stateArgs      :: [String] }
newtype Eval a = Eval {unEval :: StateT EvalState (ReaderT Scope IO) a}
    deriving (MonadState EvalState, MonadReader Scope, MonadIO, Monad)

