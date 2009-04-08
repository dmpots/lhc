{-# LANGUAGE PatternGuards #-}
module Grin.FromCore
    ( coreToGrin
    ) where

import CompactString
import qualified Language.Core  as Core
import Grin.Types      as Grin
import Grin.Pretty
import Grin.SimpleCore as Simple
import Traverse

import Data.List
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Set as Set
import qualified Data.Map as Map

import Debug.Trace

data Env
    = Env { scope :: Map.Map Variable Renamed
          , arities :: Map.Map Variable Int
          }
emptyEnv = Env Map.empty Map.empty

type M a = ReaderT Env (State Int) a

coreToGrin :: [Core.Tdef] -> [SimpleDef] -> (Grin)
coreToGrin tdefs defs
    = let gen = tdefsToNodes tdefs $ \nodes ->
                defsToFuncs defs $ \funcs ->
                get >>= \u ->
                return (Grin { grinNodes     = nodes
                             , grinFunctions = funcs
                             , grinUnique    = u
                             })
      in evalState (runReaderT gen emptyEnv) 0

tdefsToNodes :: [Core.Tdef] -> ([NodeDef] -> M a) -> M a
tdefsToNodes tdefs fn
    = bindCdefs cdefs $
      bindVariables unboxedNames $ \tuples ->
      markArities (zip unboxedNames [1..]) $
      do nodes <- mapM cdefToNode cdefs
         let unboxedTuples = flip map (zip tuples  [1..10]) $ \(tuple,n) -> NodeDef tuple (ConstructorNode 0) (replicate n PtrType)
         fn (unboxedTuples ++ nodes)
    where cdefs = tdefsToCdefs tdefs
          unboxedNames = [ qualToCompact (L.pack "ghczmprim"
                                         ,L.pack "GHCziPrim"
                                         ,L.pack $ "Z"++show n++"H") | n <- [1..10]]

tdefsToCdefs :: [Core.Tdef] -> [Core.Cdef]
tdefsToCdefs tdefs
    = concatMap tdefToCdefs tdefs

tdefToCdefs :: Core.Tdef -> [Core.Cdef]
tdefToCdefs (Core.Data _qual _tbinds cdefs)
    = cdefs
tdefToCdefs Core.Newtype{} = []

-- FIXME: Check for scoping and primitive types.
cdefToNode :: Core.Cdef -> M NodeDef
cdefToNode (Core.Constr qual _tbinds tys)
    = do name <- lookupVariable (qualToCompact qual)
         return (NodeDef name (ConstructorNode 0) (map (const PtrType) tys))

cdefName :: Core.Cdef -> CompactString
cdefName (Core.Constr qual _ _) = qualToCompact qual
cdefArity :: Core.Cdef -> Int
cdefArity (Core.Constr _ _ tys) = length tys

defsToFuncs :: [SimpleDef] -> ([FuncDef] -> M a) -> M a
defsToFuncs sdefs fn
    = bindSimpleDefs sdefs $
      do funcs <- mapM defToFunc sdefs
         fn funcs

defToFunc :: SimpleDef -> M FuncDef
defToFunc sdef
    = bindVariables (simpleDefArgs sdef) $ \renamed ->
      do exp <- strictExpression (simpleDefBody sdef)
         name <- lookupVariable (simpleDefName sdef)
         return FuncDef { funcDefName = name
                        , funcDefArgs = renamed
                        , funcDefBody = exp }

lazyExpression :: SimpleExp -> M Expression
lazyExpression simplExp
    = case simplExp of
       Simple.Case exp binding alts mbDefault ->
         bindVariable binding $ \renamed ->
           do e <- strictExpression exp
              alts' <- mapM alternative alts
              let v = Variable renamed
              return $ e :>>= v :-> Grin.Case v alts'
       Simple.Primitive p ->
         return $ Unit (Node (Builtin p) (FunctionNode 0) [])
       Var var ->
         do name <- lookupVariable var
            mbArity <- findArity var
            case mbArity of
              Nothing -> return $ Unit (Variable name)
              Just n  -> return $ Store (Node name (FunctionNode n) [])
       Dcon con ->
         do name <- lookupVariable con
            Just n <-findArity con
            --trace (show (con,a)) (return ())
            return $ Store (Node name (ConstructorNode n) [])
       Simple.Lit lit ->
         return $ Unit (Grin.Lit lit)
       Let bind func args arity e ->
         bindVariable bind $ \bind' ->
         do func' <- lookupVariable func
            args' <- mapM lookupVariable args
            e' <- lazyExpression e
            return $ Store (Node func' (FunctionNode (arity-length args)) (map Variable args')) :>>= Variable bind' :-> e'
       ap@App{} ->
         let loop acc (App a (Var b))
                 = do name <- lookupVariable b
                      mbArity <- findArity b
                      case mbArity of
                        Nothing -> loop (name:acc) a
                        Just n  -> do v <- newVariable
                                      r <- loop  (v:acc) a
                                      return $ Store (Node name (FunctionNode n) []) :>>= Variable v :-> r
{-             loop acc (App a b@App{})
                 = do e <- strictExpression b
                      v <- newVariable
                      r <- loop (v:acc) a
                      return $ e :>>= Variable v :-> r-}
             loop acc (App a b)
                 = do e <- lazyExpression b
                      v <- newVariable
                      r <- loop (v:acc) a
                      return $ e :>>= Variable v :-> r
             loop acc (Simple.Primitive p)
                 = return $ Application (Builtin p) (map Variable acc)
             loop acc (Simple.External fn conv)
                 = return $ Application (Grin.External fn) (map Variable acc)
             loop acc (Var var)
                 = do name <- lookupVariable var
                      mbArity <- findArity var
                      case mbArity of
                        Nothing -> mkApply acc name
                        Just n  -> do let (now,later) = splitAt n acc
                                      v <- newVariable
                                      ap <- mkApply later v
                                      return $ Store (Node name (FunctionNode (n-length now)) (map Variable now)) :>>= Variable v :-> ap
             loop acc (Dcon con)
                 = do name <- lookupVariable con
                      Just n <- findArity con
                      return $ Store (Node name (ConstructorNode (n-length acc)) (map Variable acc))
             loop acc e
                 = do e' <- strictExpression e
                      v  <- newVariable
                      app <- mkApply acc v
                      return (e' :>>= Variable v :-> app)
             mkApply [] v
                 = return (Unit (Variable v))
             mkApply (x:xs) v
                 = do v' <- newVariable
                      r <- mkApply xs v'
                      return $ applyCell (Variable v) (Variable x) :>>= Variable v' :-> r
         in loop [] ap
       LetRec defs e ->
         let binds = [ bind | (bind,_,_,_) <- defs ]
             funcs = [ func | (_,func,_,_) <- defs ]
             args  = [ args | (_,_,args,_) <- defs ]
             arities = [ arity | (_,_,_,arity) <- defs ] in
         bindVariables binds $ \binds' ->
         do funcs' <- mapM lookupVariable funcs
            args'  <- mapM (mapM lookupVariable) args
            e' <- strictExpression e
            let holes = foldr (\(bind,arity) b -> Store (Hole arity) :>>= Variable bind :-> b ) updates (zip binds' arities)
                updates = foldr (\(bind,fn,args,arity) b ->
                                 update bind fn args arity :>>=
                                 Empty :-> b ) e' (zip4 binds' funcs' args' arities)
            return holes
       Note _ e ->
          strictExpression e
--       Label str -> error $ "label: " ++ str
       Simple.External fn conv -> return $ Unit $ Variable $ Grin.External fn
--       DynExternal fn   -> error $ "dynexternal: " ++ fn
       _ ->
          return $ Unit Empty


{-

-- const application
fn f = f 10
fn f = eval f >>= \v -> apply v (Lit 10)

-- partial function application
fn f = f putStrLn
fn f = eval f >>= \v -> apply v (FputStrLn)

-- CAF application
fn f = f fibs
fn f = eval f >>= \v -> apply v fibs

-}

update bind fn args arity
    = Application (Builtin $ fromString "update") [Variable bind, Node fn (FunctionNode (arity-length args)) (map Variable args)]
eval v = Application (Builtin $ fromString "eval") [v]
apply a b = Application (Builtin $ fromString "apply") [a,b]
applyCell a b = Store (Node (Builtin $ fromString "apply") (FunctionNode 0) [a,b])

-- Translate a Core alternative to a Grin alternative
alternative :: Simple.Alt -> M Lambda
alternative (Acon con bs e)
    = bindVariables bs $ \renamed ->
      do e' <- lazyExpression e
         name <- lookupVariable con
         return $ Node name (ConstructorNode 0) (map Variable renamed) :-> e'
alternative (Adefault e)
    = do e' <- lazyExpression e
         v <- newVariable
         return $ Variable v :-> e'
alternative (Alit lit e)
    = do e' <- lazyExpression e
         return $ Grin.Lit lit :-> e'

strictExpression :: SimpleExp -> M Expression
strictExpression e
    = do v <- newVariable
         r <- lazyExpression e
         return $ r :>>= Variable v :-> eval (Variable v)

{-
let a = 1:b
    b = 0:a
in [a,b]

let_a b = Cons 1 b
let_b a = Cons 0 a

store Hole >>= \a ->
store Hole >>= \b ->
update a (Let_a b) >>
update b (Let_b a)


let a = 1:a
in

let_a a = Cons 1 a
a := Hole
a := Let_a a


-}



bindVariable :: Variable -> (Renamed -> M a) -> M a
bindVariable var fn
    = do u <- newUnique
         let renamed = Aliased u var
         local (\env -> env{scope = Map.insertWith errMsg var renamed (scope env)}) (fn renamed)
    where errMsg = error $ "Grin.FromCore.bindVariable: duplicate variable: " ++ show var

bindVariables :: [Variable] -> ([Renamed] -> M a) -> M a
bindVariables vs fn
    = worker [] vs
    where worker acc [] = fn (reverse acc)
          worker acc (x:xs) = bindVariable x (\r -> worker (r:acc) xs)

lookupVariable :: Variable -> M Renamed
lookupVariable var
    = asks $ \env -> Map.findWithDefault err var (scope env)
    where err = error $ "Variable not found: " ++ show var

bindSimpleDef :: SimpleDef -> M a -> M a
bindSimpleDef sdef fn
    = bindVariable (simpleDefName sdef) $ \_ ->
      markArity (simpleDefName sdef) (simpleDefArity sdef) fn

bindSimpleDefs :: [SimpleDef] -> M a -> M a
bindSimpleDefs [] = id
bindSimpleDefs (x:xs) = bindSimpleDef x . bindSimpleDefs xs

bindCdef :: Core.Cdef -> M a -> M a
bindCdef cdef fn
    = bindVariable (cdefName cdef) $ \_ ->
      markArity (cdefName cdef) (cdefArity cdef) fn

bindCdefs :: [Core.Cdef] -> M a -> M a
bindCdefs [] = id
bindCdefs (x:xs) = bindCdef x . bindCdefs xs

markArity :: Variable -> Int -> M a -> M a
markArity var arity
    = local $ \env -> env { arities = Map.insert var arity (arities env)}

markArities :: [(Variable, Int)] -> M a -> M a
markArities [] = id
markArities ((v,a):xs) = markArity v a . markArities xs

findArity :: Variable -> M (Maybe Int)
findArity var
    = asks $ \env -> Map.lookup var (arities env)

newVariable :: M Renamed
newVariable = do u <- newUnique
                 return (Anonymous u)

newUnique :: M Int
newUnique = do u <- get
               put (u+1)
               return u


