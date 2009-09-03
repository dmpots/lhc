{-# LANGUAGE PatternGuards, OverloadedStrings #-}
module Grin.FromCore
    ( coreToGrin
    ) where

import CompactString
import Grin.Types      as Grin
import Grin.SimpleCore as Simple

import qualified Grin.Lowering.GHCism as GHCism

import Data.List
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map


data Env
    = Env { scope :: Map.Map Variable Renamed
          , arities :: Map.Map Variable Int
          }
emptyEnv = Env Map.empty Map.empty

type M a = ReaderT Env (State Int) a

coreToGrin :: [SimpleType] -> [SimpleDef] -> Grin
coreToGrin tdefs defs
    = let gen = tdefsToNodes tdefs $ \nodes ->
                let (defs',cafs) = splitCAFs defs in
                bindCAFs cafs $
                defsToFuncs defs' $ \funcs ->
                defsToCAFs cafs $ \cafs' ->
                do entryPoint <- genEntryPoint
                   u <- get
                   varScope <- asks scope
                   return (GHCism.lower varScope Grin { grinNodes      = nodes
                                                      , grinCAFs       = cafs'
                                                      , grinFunctions  = entryPoint : funcs
                                                      , grinEntryPoint = funcDefName entryPoint
                                                      , grinUnique     = u
                                                      })
          genEntryPoint = do mainCaf <- lookupVariable "main::Main.main"
                             realWorld <- newVariable
                             name <- newVariable
                             v <- newVariable
                             return FuncDef { funcDefName = name
                                            , funcDefArgs = []
                                            , funcDefBody = Application (Builtin "realWorld#") [] :>>= realWorld :->
                                                            Application (Builtin "eval") [mainCaf] :>>= v :->
                                                            Application (Builtin "apply") [v,realWorld]
                                            }
      in evalState (runReaderT gen emptyEnv) 0

tdefsToNodes :: [SimpleType] -> ([NodeDef] -> M a) -> M a
tdefsToNodes tdefs fn
    = bindVariables (map simpleTypeName tdefs) $ \_ ->
      markArities [ (simpleTypeName t, simpleTypeArity t) | t <- tdefs ] $
      fn =<< mapM tdefToNode tdefs

tdefToNode :: SimpleType -> M NodeDef
tdefToNode stype
    = do name <- lookupVariable (simpleTypeName stype)
         return (NodeDef name ConstructorNode (replicate (simpleTypeArity stype) PtrType))


splitCAFs :: [SimpleDef] -> ([SimpleDef], [(Variable,Variable)])
splitCAFs []     = ([],[])
splitCAFs (x:xs)
    = let (defs,cafs) = splitCAFs xs
      in if simpleDefArity x == 0
            then let cafName = mkCAFName (simpleDefName x)
                 in ( x{simpleDefName = cafName}:defs
                    , (simpleDefName x, cafName):cafs)
            else (x:defs,cafs)

mkCAFName name = name `CompactString.append` fromString "_caf"

defsToCAFs :: [(Variable,Variable)] -> ([CAF] -> M a) -> M a
defsToCAFs vs fn
    = do cafs <- mapM defToCAF vs
         fn cafs

defToCAF :: (Variable,Variable) -> M CAF
defToCAF (varName, fnName)
    = do var <- lookupVariable varName
         fn <- lookupVariable fnName
         return $ CAF { cafName = var
                      , cafValue = Node fn FunctionNode 0 [] }

bindCAFs :: [(Variable,Variable)] -> M a -> M a
bindCAFs vs fn = bindVariables (map fst vs) $ \_ -> fn

defsToFuncs :: [SimpleDef] -> ([FuncDef] -> M a) -> M a
defsToFuncs sdefs fn
    = bindSimpleDefs sdefs $
      do funcs <- mapM defToFunc sdefs
         fn funcs

defToFunc :: SimpleDef -> M FuncDef
defToFunc sdef
    = bindVariables (simpleDefArgs sdef) $ \renamed ->
      do exp <- translate Strict (simpleDefBody sdef)
         name <- lookupVariable (simpleDefName sdef)
         return FuncDef { funcDefName = name
                        , funcDefArgs = renamed
                        , funcDefBody = exp }

data Context = Strict | Lazy

translate :: Context -> SimpleExp -> M Expression
translate cxt simplExp
    = case simplExp of
       Simple.CaseStrict exp binding alts ->
         bindVariable binding $ \renamed ->
           do e <- translate Strict exp
              alts' <- alternatives cxt alts
              return $ e :>>= renamed :-> Grin.Case renamed alts'
       Simple.Case exp binding alts | simpleExpIsPrimitive exp ->
         bindVariable binding $ \renamed ->
           do e <- translate cxt exp
              alts' <- alternatives cxt alts
              return $ e :>>= renamed :-> Grin.Case renamed alts'
       Simple.Case exp binding alts ->
         bindVariable binding $ \renamed ->
           do e <- translate Strict exp
              v <- newVariable
              alts' <- alternatives cxt alts
              return $ e :>>= v :-> Store (Variable v) :>>= renamed :-> Grin.Case v alts'
       Simple.Primitive p ->
         return $ Application (Builtin p) []
       Var var isUnboxed ->
         do name <- lookupVariable var
            mbArity <- findArity var
            case mbArity of
              Nothing -> case cxt of
                           Strict | not isUnboxed -> return $ eval name
                           _                      -> return $ Unit (Variable name)
              Just n  -> case cxt of
                           Strict -> return $ Unit (Node name FunctionNode n [])
                           Lazy   -> return $ Store (Node name FunctionNode n [])
       Dcon con ->
         do name <- lookupVariable con
            Just n <-findArity con
            case cxt of
              Strict -> return $ Unit (Node name ConstructorNode n [])
              Lazy   -> return $ Store (Node name ConstructorNode n [])
       Simple.Lit lit ->
         return $ Unit (Grin.Lit lit)
       Let bind func args arity e ->
         bindVariable bind $ \bind' ->
         do func' <- lookupVariable func
            args' <- mapM lookupVariable args
            e' <- translate cxt e
            if arity == 0
               then return $ Unit (Variable func') :>>= bind' :-> e'
               else return $ Store (Node func' FunctionNode (arity-length args) args') :>>= bind' :-> e'
       LetStrict bind fn e ->
         bindVariable bind $ \bind' ->
         do fn' <- translate Strict fn
            e' <- translate cxt e
            return $ fn' :>>= bind' :-> e'
       App fn args ->
         let process acc [] = call (reverse acc)
             process acc (x:xs)
                 = do e <- translate Lazy x
                      v <- newVariable
                      r <- process (v:acc) xs
                      return $ e :>>= v :-> r
             call vs = case fn of
                         Simple.Primitive p
                             | isBooleanPrimitive p, Lazy <- cxt
                               -> do n <- newVariable
                                     return $ Application (Builtin p) vs :>>= n :-> Store (Variable n)
                             | otherwise   -> return $ Application (Builtin p) vs
                         Simple.External e _ _ -> return $ Application (Grin.External e) vs
                         Var var isUnboxed -> do name <- lookupVariable var
                                                 mbArity <- findArity var
                                                 case mbArity of
                                                   Nothing -> case cxt of Lazy -> mkApplyLazy vs name; Strict -> mkApplyStrict vs name
                                                   Just n  -> do let (now,later) = splitAt n vs
                                                                 let node = Node name FunctionNode (n-length now) now
                                                                 case cxt of
                                                                   Lazy -> do v <- newVariable
                                                                              ap <- mkApplyLazy later v
                                                                              return $ Store node :>>= v :-> ap
                                                                   Strict -> case n `compare` length vs of
                                                                               GT -> return $ Unit node
                                                                               EQ -> return $ Application name now
                                                                               LT -> do v <- newVariable
                                                                                        ap <- mkApplyStrict later v
                                                                                        return $ Store node :>>= v :-> ap
                         Dcon con | Just n <- dconIsVector con
                                  -> return $ Unit $ Vector vs
                         Dcon con -> do name <- lookupVariable con
                                        Just n <- findArity con
                                        case cxt of
                                          Strict -> return $ Unit (Node name ConstructorNode (n-length vs) vs)
                                          Lazy   -> return $ Store (Node name ConstructorNode (n-length vs) vs)
                         e -> do e' <- translate Lazy e
                                 v  <- newVariable
                                 app <- case cxt of Lazy -> mkApplyLazy vs v; Strict -> mkApplyStrict vs v
                                 return (e' :>>= v :-> app)
             mkApplyLazy [] v
                 = return $ Unit (Variable v)
             mkApplyLazy (x:xs) v
                 = do v' <- newVariable
                      r <- mkApplyLazy xs v'
                      return $ applyCell v x :>>= v' :-> r
             mkApplyStrict xs v
                 = do let loop v [] = return $ Unit (Variable v)
                          loop v (x:xs) = do v' <- newVariable
                                             r  <- loop v' xs
                                             return $ apply v x :>>= v' :-> r
                      v' <- newVariable
                      r <- loop v' xs
                      return $ eval v :>>= v' :-> r
         in process [] args
       LetRec defs e ->
         let binds = [ bind | (bind,_,_,_) <- defs ]
             funcs = [ func | (_,func,_,_) <- defs ]
             args  = [ args | (_,_,args,_) <- defs ]
             arities = [ arity | (_,_,_,arity) <- defs ] in
         bindVariables binds $ \binds' ->
         do funcs' <- mapM lookupVariable funcs
            args'  <- mapM (mapM lookupVariable) args
            e' <- translate cxt e
            vars <- replicateM (length defs) newVariable
            let holes = foldr (\(bind,arity) b -> Store (Hole arity) :>>= bind :-> b ) updates (zip binds' arities)
                updates = foldr (\(bind,fn,args,arity,var) b ->
                                 update bind fn args arity var :>>
                                 b ) e' (zip5 binds' funcs' args' arities vars)
            return holes
       Note _ e ->
          translate cxt e
--       Label str -> error $ "label: " ++ str
       Simple.External fn conv _ -> return $ Unit $ Variable $ Grin.External fn
--       DynExternal fn   -> error $ "dynexternal: " ++ fn
       _ ->
          return $ Unit Empty

dconIsVector con
    = Map.lookup con (Map.fromList vectors)
    where vectors = [ (fromString "ghc-prim:GHC.Prim.(# #)", 1)
                    , (fromString "ghc-prim:GHC.Prim.(#,#)", 2)
                    , (fromString "ghc-prim:GHC.Prim.(#,,#)", 3)
                    , (fromString "ghc-prim:GHC.Prim.(#,,,#)", 4)
                    , (fromString "ghc-prim:GHC.Prim.(#,,,,#)", 5)
                    , (fromString "ghc-prim:GHC.Prim.(#,,,,,#)", 6)
                    , (fromString "ghc-prim:GHC.Prim.(#,,,,,,#)", 7)
                    , (fromString "ghc-prim:GHC.Prim.(#,,,,,,,#)", 8)
                    ]


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

update bind fn args arity var
    = Unit (Node fn FunctionNode (arity-length args) args) :>>= var :->
      Application (Builtin $ fromString "update") [bind, var]
eval v = Application (Builtin $ fromString "eval") [v]
apply a b = Application (Builtin $ fromString "apply") [a,b]
applyCell a b = Store (Node (Builtin $ fromString "evalApply") FunctionNode 0 [a,b])

alternatives :: Context -> [Simple.Alt] -> M [Grin.Alt]
alternatives cxt alts
    = mapM (alternative (translate cxt)) (others ++ defaults)
    where isDefault Adefault{} = True
          isDefault _          = False
          (defaults,others)    = partition isDefault alts

-- Translate a Core alternative to a Grin alternative
alternative :: (SimpleExp -> M Expression) -> Simple.Alt -> M Grin.Alt
alternative fn (Acon con bs e) | Just n <- dconIsVector con
    = bindVariables bs $ \renamed ->
      do e' <- fn e
         return $ Vector renamed :> e'
alternative fn (Acon con bs e)
    = bindVariables bs $ \renamed ->
      do e' <- fn e
         name <- lookupVariable con
         return $ Node name ConstructorNode 0 renamed :> e'
alternative fn (Adefault e)
    = do e' <- fn e
         v <- newVariable
         return $ Variable v :> e'
alternative fn (Alit lit e)
    = do e' <- fn e
         return $ Grin.Lit lit :> e'

simpleExpIsPrimitive :: SimpleExp -> Bool
simpleExpIsPrimitive (App (Simple.Primitive prim) _) | isBooleanPrimitive prim
    = False
simpleExpIsPrimitive (App Simple.Primitive{} _)
    = True
simpleExpIsPrimitive (App Simple.External{} _)
    = True
simpleExpIsPrimitive Simple.Lit{} = True
simpleExpIsPrimitive _
    = False

isBooleanPrimitive x = x `elem` [">=#",">#","==#","/=#","<=#","<#","<##",">##",">=##","<=##","==##"
                                ,"eqWord#", "neWord#", "leWord#"]


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


