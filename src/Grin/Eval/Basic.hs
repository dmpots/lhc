module Grin.Eval.Basic where

import CompactString
import Grin.Types hiding (Value(..))
import qualified Grin.Types as Grin
import Grin.Pretty

import Control.Monad (ap)
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

import Debug.Trace

eval :: Grin -> String -> EvalValue
eval grin entry
    = runEval grin $
      do ptr <- storeValue =<< callFunction renamedEntry []
         callFunction (Builtin $ fromString "apply") [HeapPointer ptr, Empty]
    where renamedEntry = case [ renamed | FuncDef{funcDefName = renamed@(Aliased _ name)} <- grinFunctions grin, name == fromString entry ] of
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
      deriving (Show)
data EvalState
    = EvalState { stateFunctions :: Map.Map Renamed FuncDef
                , stateHeap      :: Heap
                , stateFree      :: HeapPointer }
type Eval a = StateT EvalState (Reader Scope) a

runEval :: Grin -> Eval a -> a
runEval grin fn
    = runReader (evalStateT fn initState) emptyScope
    where emptyScope = Map.empty
          initState = EvalState { stateFunctions = Map.fromList [ (funcDefName def, def) | def <- grinFunctions grin ]
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
           _ -> error (show fn)
callFunction (Builtin fnName) [fn, handler, realWorld] | fnName == fromString "catch#"
    = callFunction (Builtin $ fromString "apply") [fn, realWorld]
callFunction (Builtin fnName) [realWorld] | fnName == fromString "noDuplicate#"
    = return $ realWorld
callFunction (Builtin fnName) [] | fnName == fromString "realWorld#"
    = return Empty
callFunction (External "__hscore_bufsiz") [realWorld]
    = return $ Lit (Lint 512)
callFunction fnName args
    = do fn <- lookupFunction fnName
         ret <- trace ("Calling: " ++ show fnName) $ runFunction fn args
         trace ("calling: " ++ show fnName ++ ", returning: " ++ show ret) $ return ret

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
    = do val' <- trace ("Case for: " ++ show (ppValue val)) $ toEvalValue val
         runCase val' alts
runExpression e = error (show (ppExpression e))

runCase :: EvalValue -> [Lambda] -> Eval EvalValue
runCase val@(Node nodeName _type args) alts
    = worker alts
    where worker [] = error $ "Grin.Eval.Basic.runCase: no match found."
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
runCase val alts = error (show (val,length alts))



runEvalPrimitive (HeapPointer ptr) = runEvalPrimitive =<< fetch ptr
runEvalPrimitive (Node fn (FunctionNode 0) args) = callFunction fn args
runEvalPrimitive val = return val

storeValue :: EvalValue -> Eval HeapPointer
storeValue val
    = do st <- get
         let newFree = stateFree st + 1
         put st{stateFree = newFree
               ,stateHeap = Map.insert (stateFree st) val (stateHeap st)}
         return (stateFree st)

bindLambda :: EvalValue -> Grin.Value -> Eval a -> Eval a
bindLambda (Node tag _ args) (Grin.Node bTag _ bArgs) | length args == length bArgs
    = bindLambdas (zip args bArgs)
bindLambda from (Grin.Variable to)
    = bindValue to from
bindLambda from to
    = error (show (from, to))

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
