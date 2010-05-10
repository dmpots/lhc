{-# LANGUAGE OverloadedStrings, PatternGuards#-}
{-|
  This module contains the datatype for the abstract heap and environment used
  in the heap-points-to analysis. The description is based on the paper
  /GRIN: a Highly Optimizing Back End for Lazy Functional Languages/ by Boquist
  and Johnsson.

  The points two analysis operates over two abstract program structures.

    * The abstract /environment/ contains the variables in the GRIN program plus one
  variable for each procedure which denotes the return value for that procedure.

    * The abstract /heap/ has one entry for each @store@ that occurs in the GRIN
  program. This entry points to the set of possible nodes that can be stored in
  that location.

  /Initial Environment/

  The initial enviornment is created with the 'mkEnvironment' function. It
  creates the initial mapping from 'Lhs' to 'RhsValue'. The enviornment is
  initialized as follows.

    * Each variable that is a basic value gets mapped to the 'Base' constructor.
      A variable gets a basic value from a statement like @v <- unit 3@.

    * For each @store@ in the program a new heap pointer is created. The /heap/
      ('HeapEntry') maps this pointer to the value ('RhsValue') being stored. The
      /environment/ maps the bound varaible ('VarEntry') returned from the store
      to the new heap pointer ('Heap').

    * CAFs are treated as a store of the CAF value that defines the CAF varaible.

    * Formal parameter variables to functions get the union of all the actual
      arguments ('Ident') used in calls to that function. This includes both
      lazy calls represented by constructing @(Ffun ...)@ nodes and direct calls
      to the function.

    * Variables bound as the result of an @v <- eval p@ get the @'Eval' p@
      abstract value. The meaning of @Eval p@ is to take the union of all the
      abstract heap locations that @p@ might point to and then extracting all
      the 'RhsValue's that correspond to a 'Tag' entry. Thus @v@ will have all
      of the possible constructors returned as a result of the @eval@.

    * Variables bound in a case statement get the value by extracting the value
      from the corresponding component of the variable being cased. For pattern
      matching of constructors, the 'Extract' is used to get the corresponding
      variable for the pattern match. For literals, no new equations are added.
      For variables in the case (i.e. @case v  of  v' -> ...@), the variable @v'@
      gets the same value as the variable @v@ being cased.

  /Dealing with Higer Order Functions/

  Higher order functions are handled with the @apply@ primitive. We need to
  handle two cases with higher order functions.

    * The abstract value of @apply f a@. This value depends of the value of @f@.
      If @f@ contains a function node that still needs > 1 argument then the
      result of the apply is the same function node with one fewer missing
      arguments. If @f@ contains a function node that is missing one argument
      then the abstract value of the @apply@ is the same as the abstract value
      returned by the function call. We use an @'Apply' f a@ node to denote the
      abstract value of an @apply@ primitive. It will be intpreted as described
      above when the equations are solved.

    * The abstract values of formal parameters. With higher order functions the
      formal parameters of functions need to include values that are passed to
      the function in a call to the @apply@ primitive. We use a single abstract
      variable (say @applications@), to track the effects of @apply@. For each
      @apply f a@ in the program we add a @'PartialApplication'f a@ to the rhs
      of the the @applications@ variable.  When solving the equations, if a
      'PartialApplication' would result in a fully saturated call, we add a
      @'Tag' f ... a ...@ to the rhs of @applications@. Finally the equation for
      each the @nth@ formal parameter to a function needs to add an
      @'Extract applications (f,...) n'@ component to the rhs of its
      equation. The extract will pull out the abstract value for the @nth@
      argument to the function that occurs from all calls to that function
      through the @apply@ primitive.


-}
module Grin.HPT.Environment
    ( mkEnvironment
    , Equations
    , Rhs(..)
    , RhsValue(..)
    , HeapPointer
    , Lhs(..)
    , Node
    , singleton
    , isSubsetOf
    ) where

import Grin.Types hiding (Update)
import qualified Grin.Types as Grin

import qualified Data.Map as Map
import Control.Monad.RWS

import Control.Parallel.Strategies

import LHC.Prim

type HeapPointer = Int
-- | Left hand side of the heap points to equations. We model both the
--   environment and the  heap. The envirnement tracks values for each variable
--   in the program. The heap tracks values for each abstract heap location.
data Lhs = HeapEntry HeapPointer -- ^ Heap location
         | VarEntry Renamed      -- ^ Variable
    deriving (Eq,Ord,Show)

instance NFData Lhs where
    rnf (HeapEntry hp) = ()
    rnf (VarEntry r) = ()

type Node = (Renamed, NodeType, Int) -- Name, node type, missing arguments.


-- | Possible entries on the right hand side of a heap points to equation. The
--   actual 'Rhs' with be a set of these values.
data RhsValue
      -- | Extract the abstract nth parameter for a given tag from the set of values (e.g. the \downarrow operator).
      --   @Renamed@ = named set of values (e.g. a varaible on the Lhs)
      --   Node = the tag of the node to extract
      --   Int = the position of the parameter in the Grin node
    = Extract Renamed Node Int
    | ExtractVector Renamed Int      -- ^ Extract a position from a vector appearing on the Lhs with name @Renamed@
    | Eval Renamed                   -- ^ An application of the @eval@ primitive
    | Update Renamed Renamed         -- ^ An application of the @update@ primitive
    | Apply Renamed Renamed          -- ^ An application of the @apply@ primitive
    | PartialApply Renamed Renamed   -- ^ An application of the @apply@ primitive (to remembering partial applications)
    | Ident Renamed                  -- ^ An identifier of another Lhs value
    | Fetch Renamed                  -- ^ An application of the @fetch@ primitive
    | Base                           -- ^ A basic value
    | Heap HeapPointer               -- ^ A pointer to a heap location
    | Tag Renamed NodeType Int [Rhs] -- ^ A function or constructor grin node
    | VectorTag [Rhs]                -- ^ An unboxed tuple value
    deriving (Eq,Ord,Show)

-- A set of possible rhs values
newtype Rhs = Rhs [RhsValue] deriving (Eq,Ord,Show)
singleton :: RhsValue -> Rhs
singleton value = Rhs [value]

type Equations = Map.Map Lhs Rhs



instance Monoid Rhs where
    mempty = Rhs []
    mappend (Rhs a) (Rhs b)
        = Rhs (worker a b)
        where worker [] lst = lst
              worker lst [] = lst
              worker (Tag tag1 nt1 missing1 args1:xs) (Tag tag2 nt2 missing2 args2:ys)
                  = case tag1 `compare` tag2 of
                      LT -> Tag tag1 nt1 missing1 args1 : worker xs (Tag tag2 nt2 missing2 args2:ys)
                      GT -> Tag tag2 nt2 missing2 args2 : worker (Tag tag1 nt1 missing1 args1:xs) ys
                      EQ -> Tag tag1 nt1 (min missing1 missing2) (zipJoin args1 args2):worker xs ys
              worker (VectorTag v1:xs) (VectorTag v2:ys)
                  = VectorTag (zipJoin v1 v2) : worker xs ys
{-              worker (y@Tag{}:ys) (x:xs)
                  = x:worker (y:ys) xs
              worker (y:ys) (x@Tag{}:xs)
                  = y:worker ys (x:xs)-}
              worker (y:ys) (x:xs)
                  = case y `compare` x of
                      LT -> y:worker ys (x:xs)
                      GT -> x:worker (y:ys) xs
                      EQ -> x:worker ys xs

isSubsetOf :: Rhs -> Rhs -> Bool
Rhs lRhs `isSubsetOf` Rhs rRhs
    = worker lRhs rRhs
    where worker [] y  = True
          worker x [] = False
          worker (x@(Tag tag1 _ _ args1):xs) (y@(Tag tag2 _ _ args2):ys)
              = case tag1 `compare` tag2 of
                  LT -> False
                  GT -> worker (x:xs) ys
                  EQ -> and (zipWith isSubsetOf args1 args2) && worker xs ys
          worker (x:xs) (y:ys)
              = case x `compare` y of
                  LT -> False
                  GT -> worker (x:xs) ys
                  EQ -> worker xs ys

zipJoin :: Monoid a => [a] -> [a] -> [a]
zipJoin [] []         = []
zipJoin [] lst        = zipWith mappend (repeat mempty) lst
zipJoin lst []        = zipWith mappend lst (repeat mempty)
zipJoin (x:xs) (y:ys) = mappend x y : zipJoin xs ys


type GenReader = Map.Map Renamed [Renamed] -- map from functions to function args
type GenM a = RWS GenReader (Endo Equations) HeapPointer a


applications :: Renamed
applications = Builtin "applications"

updates :: Renamed
updates = Builtin "updates"

mkEnvironment :: Grin -> Equations
mkEnvironment grin
    = case execRWS (setupEnvGrin grin) reader 0 of
         (st, eqsEndo) -> appEndo eqsEndo Map.empty
    where reader = Map.fromList [ (funcDefName func, funcDefArgs func) | func <- grinFunctions grin ]

setupEnvGrin :: Grin -> GenM ()
setupEnvGrin grin
    = do forM_ (grinCAFs grin) $ \caf ->
           do hp <- store =<< processVal (cafValue caf)
              addEquation (VarEntry (cafName caf)) (singleton $ Heap hp)
         forM_ (grinFunctions grin) $ \function ->
           do rhs <- setupEnv (funcDefBody function)
              addEquation (VarEntry (funcDefName function)) rhs
              forM_ (zip (funcDefArgs function) [0..]) $ \(arg, n) ->
                addEquation (VarEntry arg) -- For applications resulting from the apply primitive
                            (singleton $ Extract applications (funcDefName function, FunctionNode, 0) n)

setupEnv :: Expression -> GenM Rhs
setupEnv (Store val)
    = do hp <- store =<< processVal val
         return $ singleton $ Heap hp
setupEnv (exp :>>= bind :-> rest)
    = do expRhs <- setupEnv exp
         addEquation (VarEntry bind) expRhs
         setupEnv rest
setupEnv (exp :>> rest)
    = do setupEnv exp
         setupEnv rest
setupEnv (Unit val)
    = processVal val
setupEnv (Case val alts)
    = do let valRhs = singleton $ Ident val
         rets <- forM alts $ \(l :> alt) ->
                   case l of
                     Node tag nt missing args
                       -> do forM_ (zip [0..] args) $ \(n,arg) ->
                               addEquation (VarEntry arg) (singleton $ Extract val (tag, nt, missing) n)
                             setupEnv alt
                     Vector args -> do forM_ (zip [0..] args) $ \(n,arg) ->
                                         addEquation (VarEntry arg) (singleton $ ExtractVector val n)
                                       setupEnv alt
                     Lit{}          -> setupEnv alt
                     Variable v     -> do addEquation (VarEntry v) valRhs
                                          setupEnv alt
                     _              -> error $ "setupEnv: Invalid case: " ++ show l
         return $ mconcat rets
setupEnv (Application External{} args)
    = return $ singleton (VectorTag [singleton Base, singleton Base])

setupEnv (Application (Builtin "eval") [arg])
  = do return $ singleton (Eval arg)
setupEnv (Application (Builtin "apply") [arg1, arg2])
  = do addEquation (VarEntry applications) (singleton $ PartialApply arg1 arg2)
       return $ singleton (Apply arg1 arg2)
--setupEnv (Application (Builtin "update") [ptr,val])
--    = do addEquation (VarEntry updates) (singleton $ Update ptr val)
--           return mempty
setupEnv (Grin.Update size ptr val)
    = do addEquation (VarEntry updates) (singleton $ Update ptr val)
         return mempty
setupEnv (Application (Builtin "updateMutVar") [ptr, val, realWorld])
    = do addEquation (VarEntry updates) (singleton $ Update ptr val)
         return $ singleton Base

setupEnv (Application (Builtin fn) args) | fn `elem` baseBuiltins
    = return $ singleton Base
setupEnv (Application (Builtin fn) args) | Just len <- fn `lookup` vectorBuiltins 
    = return $ singleton $ VectorTag $ take len $ repeat (singleton Base)
setupEnv (Application (Builtin fn) args) | fn `elem` unsupportedBuiltins
    = return mempty

setupEnv (Application (Builtin "makeStablePtr#") [val,realworld])
    = do hp <- store (singleton $ Ident val)
         return $ singleton $ VectorTag [singleton Base, singleton $ Heap hp]
setupEnv (Application (Builtin "deRefStablePtr#") [ptr,realworld])
    = do return $ singleton $ VectorTag [singleton Base, singleton $ Fetch ptr]
setupEnv (Application (Builtin "unblockAsyncExceptions#") [fn, realworld])
    = do return $ singleton $ Apply fn realworld
setupEnv (Application (Builtin "blockAsyncExceptions#") [fn, realworld])
    = do return $ singleton $ Apply fn realworld
setupEnv (Application (Builtin "fetch") [a])
    = return $ singleton $ Fetch a
setupEnv (Application (Builtin "newArray#") [size, elt, realworld])
    = do hp <- store (singleton $ Ident elt)
         return $ singleton $ VectorTag [singleton Base, singleton $ Heap hp]
setupEnv (Application (Builtin "readArray#") [arr, nth, realworld])
    = return $ singleton $ VectorTag [singleton Base, singleton $ Fetch arr]
setupEnv (Application (Builtin "unsafeFreezeArray#") [arr, realworld])
    = return $ singleton $ VectorTag [singleton Base, singleton $ Ident arr]
setupEnv (Application (Builtin "indexArray#") [arr, nth])
    = return $ singleton $ VectorTag [singleton $ Fetch arr ]
setupEnv (Application (Builtin "writeArray#") [arr, nth, elt, realworld])
    = do addEquation (VarEntry updates) (singleton $ Update arr elt)
         return (singleton Base)
setupEnv (Application (Builtin "mkWeak#") [o,val,c,realworld])
    = do hp <- store (singleton $ Ident val)
         return $ singleton $ VectorTag [singleton Base, singleton $ Heap hp]
setupEnv (Application (Builtin "deRefWeak#") [ptr,realworld])
    = do return $ singleton $ VectorTag [singleton Base, singleton Base, singleton $ Fetch ptr]
setupEnv (Application (Builtin builtin) args)
    = error $ "unknown builtin: " ++ show builtin

setupEnv (Application fn args)
    = do funcArgs <- lookupFuncArgs fn
         forM_ (zip funcArgs args) $ \(formal, actual) -> -- Add abstract value of actual arg to the formal arg
           addEquation (VarEntry formal) (singleton $ Ident actual)
         return $ singleton (Ident fn)




processVal :: Value -> GenM Rhs
processVal (Node name nt missing args)
    = do case nt of
           FunctionNode ->
             do funcArgs <- lookupFuncArgs name
                forM_ (zip funcArgs args) $ \(formal,actual) -> -- Add abstract value of actual arg to the formal arg
                  addEquation (VarEntry formal) (singleton $ Ident actual)
           ConstructorNode ->
             do return ()
         return $ singleton $ Tag name nt missing (map (singleton . Ident) args)

processVal (Variable var) = return $ singleton $ Ident var
processVal Lit{}          = return $ singleton Base
processVal Hole{}         = return mempty
processVal Empty          = return mempty
processVal (Vector vs)    = return $ singleton $ VectorTag (map (singleton . Ident) vs)


-- TODO: if the value stored is a function application, meet the abstract
--       value of the function (representing its return value) to the heap pointer
--       value.
store :: Rhs -> GenM Int
store rhs
  = do u <- get
       put $ u+1
       addEquation (HeapEntry u) rhs
       return u

addEquation :: Lhs -> Rhs -> GenM ()
addEquation lhs rhs
    = tell $ Endo $ Map.insertWith mappend lhs rhs


lookupFuncArgs :: Renamed -> GenM [Renamed]
lookupFuncArgs func
    = asks $ \funcs ->
      case Map.lookup func funcs of
        Nothing   -> error $ "Couldn't find function: " ++ show func
        Just args -> args


