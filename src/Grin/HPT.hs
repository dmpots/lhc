{-# LANGUAGE OverloadedStrings #-}
module Grin.HPT
    ( analyze
    , lower
    ) where

import CompactString
import Grin.Types

import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Debug.Trace
import System.IO
import System.IO.Unsafe

type HeapPointer = Int
data Lhs = HeapEntry HeapPointer
         | VarEntry Renamed
    deriving (Eq,Ord)

data RhsValue
    = Extract Rhs Renamed Int
    | ExtractVector Rhs Int
    | Eval Renamed
    | Update Renamed Renamed
    | Apply Renamed Renamed
    | PartialApply Renamed Renamed
    | Ident Renamed
    | Fetch Renamed
    | Base
    | Heap HeapPointer
    | Tag Renamed NodeType Int [Rhs]
    | VectorTag [Rhs]
    deriving (Eq,Ord,Show)

-- A set of possible rhs values
newtype Rhs = Rhs [RhsValue] deriving (Eq,Ord,Show)
singleton :: RhsValue -> Rhs
singleton value = Rhs [value]

fromList :: [RhsValue] -> Rhs
fromList ls = mconcat $ map singleton ls


type Equations = Map.Map Lhs Rhs
ppEquations :: Equations -> String
ppEquations eqs
    = unlines [ unwords [ppLhs lhs, "=", show rhs] | (lhs,rhs) <- Map.toList eqs ]
    where ppLhs (HeapEntry hp) = "hp:"++show hp
          ppLhs (VarEntry var) = show var


data HeapAnalysis
    = HeapAnalysis (Map.Map Lhs Rhs)




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
              worker (y@Tag{}:ys) (x:xs)
                  = x:worker (y:ys) xs
              worker (y:ys) (x@Tag{}:xs)
                  = y:worker ys (x:xs)
              worker (y:ys) (x:xs)
                  = case y `compare` x of
                      LT -> y:worker ys (x:xs)
                      GT -> x:worker (y:ys) xs
                      EQ -> x:worker ys xs

zipJoin [] []         = []
zipJoin [] lst        = zipWith mappend (repeat mempty) lst
zipJoin lst []        = zipWith mappend lst (repeat mempty)
zipJoin (x:xs) (y:ys) = mappend x y : zipJoin xs ys


type GenReader = Map.Map Renamed [Renamed]
type GenM a = RWS GenReader (Endo Equations) Int a

applications :: Renamed
applications = Builtin "applications"
applicationsRhs :: Rhs
applicationsRhs = singleton $ Ident applications

updates :: Renamed
updates = Builtin "updates"

setupEnvGrin :: Grin -> GenM ()
setupEnvGrin grin
    = do forM_ (grinCAFs grin) $ \caf ->
           do hp <- store =<< processVal (cafValue caf)
              addEquation (VarEntry (cafName caf)) (singleton $ Heap hp)
         forM_ (grinFunctions grin) $ \function ->
           do rhs <- setupEnv (funcDefBody function)
              addEquation (VarEntry (funcDefName function)) rhs
              forM_ (zip (funcDefArgs function) [0..]) $ \(arg, n) ->
                addEquation (VarEntry arg)
                            (singleton $ Extract applicationsRhs (funcDefName function) n)

setupEnv :: Expression -> GenM Rhs
setupEnv (Store val)
    = do hp <- store =<< processVal val
         return $ singleton $ Heap hp
setupEnv (exp :>>= bind :-> rest)
  = do expRhs <- setupEnv exp
       case bind of
         Variable l      -> addEquation (VarEntry l) expRhs
         Node tag _ _ args -> forM_ (zip args [0..]) $ \(arg, n) ->
                                addEquation (VarEntry arg) (singleton $ Extract expRhs tag n)
         Vector vs       -> forM_ (zip vs [0..]) $ \(arg, n) ->
                              addEquation (VarEntry arg) (singleton $ ExtractVector expRhs n)
         Empty           -> return ()
       setupEnv rest
setupEnv (Unit val)
    = processVal val
setupEnv (Case val alts)
    = do valRhs <- processVal val
         rets <- forM alts $ \(l :-> alt) ->
                   case l of
                     Node tag _ _ args -> do forM_ (zip [0..] args) $ \(n,arg) ->
                                               addEquation (VarEntry arg) (singleton $ Extract valRhs tag n)
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
setupEnv (Application (Builtin "update") [ptr,val])
    = do addEquation (VarEntry updates) (singleton $ Update ptr val)
         return mempty
setupEnv (Application (Builtin fn) args) | fn `elem` ["<#",">#","<=#",">=#","-#","+#","*#","narrow32Int#"
                                                     ,"uncheckedIShiftRA#","and#","==#", "remInt#", "noDuplicate#"
                                                     ,"readInt8OffAddr#","narrow8Word#", "writeInt8OffAddr#"
                                                     ,"narrow8Int#", "newPinnedByteArray#", "byteArrayContents#","touch#"
                                                     ,"newAlignedPinnedByteArray#", "uncheckedIShiftL#", "negateInt#"
                                                     ,"indexCharOffAddr#","minusWord#","writeTVar#","geWord#"
                                                     ]
    = return $ singleton Base
setupEnv (Application (Builtin fn) args) | fn `elem` ["unsafeFreezeByteArray#"
                                                     ,"atomically#", "word2Integer#","integer2Int#"]
    = return $ singleton $ VectorTag [singleton Base, singleton Base]
setupEnv (Application (Builtin "makeStablePtr#") [val,realworld])
    = do hp <- store (singleton $ Ident val)
         return $ singleton $ VectorTag [singleton Base, singleton $ Heap hp]
setupEnv (Application (Builtin "deRefStablePtr#") [ptr,realworld])
    = do return $ singleton $ VectorTag [singleton Base, singleton $ Fetch ptr]
setupEnv (Application (Builtin "unblockAsyncExceptions#") [fn, realworld])
    = do return $ singleton $ Apply fn realworld
setupEnv (Application (Builtin "blockAsyncExceptions#") [fn, realworld])
    = do return $ singleton $ Apply fn realworld
setupEnv (Application (Builtin "catch#") [fn, handler, realworld])
    = do return $ singleton $ Apply fn realworld
setupEnv (Application (Builtin "fetch") [a])
    = return $ singleton $ Fetch a
setupEnv (Application (Builtin "newArray#") [size, elt, realworld])
    = do hp <- store (singleton $ Ident elt)
         return $ singleton $ VectorTag [singleton Base, singleton $ Heap hp]
setupEnv (Application (Builtin "readArray#") [arr, nth, realworld])
    = return $ singleton $ VectorTag [singleton Base, singleton $ Fetch arr]
setupEnv (Application (Builtin "writeArray#") [arr, nth, elt, realworld])
    = do addEquation (VarEntry updates) (singleton $ Update arr elt)
         return mempty
setupEnv (Application (Builtin _) args)
    = return mempty
setupEnv (Application (Builtin builtin) args)
    = error $ "unknown builtin: " ++ show builtin

setupEnv (Application fn args)
    = do funcArgs <- lookupFuncArgs fn
         forM_ (zip funcArgs args) $ \(var, arg) ->
           addEquation (VarEntry var) (singleton $ Ident arg)
         return $ singleton (Ident fn)




processVal :: Value -> GenM Rhs
processVal (Node name nt missing args)
    = do case nt of
           FunctionNode ->
             do funcArgs <- lookupFuncArgs name
                forM_ (zip funcArgs args) $ \(funcArg,arg) ->
                  addEquation (VarEntry funcArg) (singleton $ Ident arg)
           ConstructorNode ->
             do return ()
         return $ singleton $ Tag name nt missing (map (singleton . Ident) args)
processVal (Variable var) = return $ singleton $ Ident var
processVal Lit{}          = return $ singleton Base
processVal Hole{}         = return mempty
processVal Empty          = return mempty
processVal (Vector vs)    = return $ singleton $ VectorTag (map (singleton . Ident) vs)

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


solve :: Equations -> (Int, Equations)
solve eqs
    = let iterate ls
              = forM_ ls $ \(lhs,rhs) ->
                  do dead <- isDead lhs
                     when (not dead) $
                       do reducedRhs <- reduceEqs rhs
                          addReduced lhs reducedRhs
                          --dead <- rhsIsDead rhs
                          {-traceOut (if dead then "" else "\nNot dead: " ++ show rhs ++ "\n") $ -}
                          --when dead $ setDead lhs
          loop iter dead prev
              = case {-traceOut ("\nIteration: " ++ show iter ++ "\n") $-} (execWriter (runReaderT (iterate (Map.toList eqs)) (dead,prev))) of
                  (newDead,newDefs) ->
                    let next = (Map.unionWith mappend prev (appEndo newDefs Map.empty))
                    in if prev == next then (iter, next) else loop (iter+1) (appEndo newDead dead) next
      in loop 1 (Map.map (const False) eqs) (Map.map (const mempty) eqs)

traceOut str v = unsafePerformIO (putStr str) `seq` v

rhsIsDead (Rhs vs) = liftM and $ mapM valueIsDead vs
    where valueIsDead (Ident i) = isDead (VarEntry i)
          valueIsDead (Heap hp) = isDead (HeapEntry hp)
          valueIsDead Base      = return True
          valueIsDead (Eval i)  = isDead (VarEntry i)
          valueIsDead (Apply a b) = isDead (VarEntry a)
          valueIsDead (Tag _fn FunctionNode n _args) | n >= 1 = return True
          valueIsDead (Tag _fn _type _n args)
              = liftM and $ mapM rhsIsDead args
          valueIsDead (VectorTag args)
              = liftM and $ mapM rhsIsDead args
          valueIsDead (Extract rhs tag n)
              = rhsIsDead rhs
          valueIsDead (ExtractVector rhs n)
              = rhsIsDead rhs
          valueIsDead _ = return False


a `isSubSetOf` b = b == (a `mappend` b)

addReduced lhs rhs
    = do orig <- lookupEq lhs
         let isNew = not (rhs `isSubSetOf` orig)
             tag = if isNew then "+" else "-"
         {-traceOut tag $ unless (rhs `isSubSetOf` orig) $ -}
         tell $ (mempty, Endo $ Map.insertWith mappend lhs rhs)

isDead lhs = asks $ \(dead,_) -> Map.findWithDefault False lhs dead
setDead lhs = tell $ (Endo $ Map.insert lhs True, mempty)

reduceEqs (Rhs rhs) = do rhs' <- mapM reduceEq rhs
                         return $ mconcat rhs'

reduceEq Base      = return $ singleton Base
reduceEq (Heap hp) = return $ singleton $ Heap hp
reduceEq (Ident i) = lookupEq (VarEntry i)
reduceEq (Extract eqs tag n)
    = do Rhs eqs' <- reduceEqs eqs
         reduceEqs (mconcat [ args `nth` n | Tag t _ _ args <- eqs', t == tag ])
    where nth [] n = mempty --error $ "reduceEq: ExtractVector: " ++ show (eqs, tag, n)
          nth (x:xs) 0 = x
          nth (x:xs) n = nth xs (n-1)
reduceEq (ExtractVector eqs n)
    = do Rhs eqs' <- reduceEqs eqs
         reduceEqs (mconcat [ args `nth` n | VectorTag args <- eqs' ])
    where nth [] n = error $ "reduceEq: ExtractVector: " ++ show (eqs, n)
          nth (x:xs) 0 = x
          nth (x:xs) n = nth xs (n-1)
reduceEq (Tag fn FunctionNode 0 args)
    = do args' <- mapM reduceEqs args
         rets <- lookupEq (VarEntry fn)
         return $ singleton (Tag fn FunctionNode 0 args') `mappend` rets
reduceEq (Tag t nt missing args)
    = do --args' <- mapM reduceEqs args
         return $ singleton (Tag t nt missing args)
reduceEq (VectorTag args)
    = do args' <- mapM reduceEqs args
         return $ singleton (VectorTag args')
reduceEq (Eval i)
    = do Rhs vals <- lookupEq (VarEntry i)
         let f (Heap hp) = do Rhs rhs <- lookupEq (HeapEntry hp)
                              let worker (Tag fn FunctionNode 0 _) = lookupEq (VarEntry fn)
                                  worker other = return $ singleton other
                              liftM mconcat $ mapM worker rhs
             f Base = return mempty
             f t = error $ "reduceEq: eval: " ++ show (t,i,vals)
         liftM mconcat $ mapM f vals
reduceEq (Fetch i)
    = do Rhs vals <- lookupEq (VarEntry i)
         let f (Heap hp) = lookupEq (HeapEntry hp)
             f Base      = return mempty
             f t = error $ "reduceEq: fetch: " ++ show (t,i,vals)
         liftM mconcat $ mapM f vals
reduceEq (Apply a b)
    = do Rhs vals <- lookupEq (VarEntry a)
         let f (Tag func FunctionNode 1 args)
                 = reduceEq (Ident func)
             f (Tag conc nt n args)
                 | n == 0    = return mempty
                 | otherwise = return $ singleton (Tag conc nt (n-1) (args ++ [singleton (Ident b)]))
             f t             = error $ "reduceEq: apply: " ++ show t
         liftM mconcat $ mapM f vals
reduceEq (PartialApply a b)
    = do Rhs vals <- lookupEq (VarEntry a)
         let f (Tag tag nt n args)
                 | n == 0    = return mempty
                 | otherwise = return $ singleton (Tag tag nt (n-1) (args ++ [singleton (Ident b)]))
             f t             = error $ "reduceEq: apply: " ++ show t
         liftM mconcat $ mapM f vals
reduceEq (Update hp val)
    = do Rhs hps <- lookupEq (VarEntry hp)
         valRhs  <- lookupEq (VarEntry val)
         forM_ hps $ \(Heap hp) -> addReduced (HeapEntry hp) valRhs
         return mempty

lookupEq lhs
    = asks $ \(_,eqs) -> Map.findWithDefault mempty lhs eqs



analyze :: Grin -> (Int, HeapAnalysis)
analyze grin
    = let reader      = Map.fromList [ (funcDefName func, funcDefArgs func) | func <- grinFunctions grin ] in
      case execRWS (setupEnvGrin grin) reader 0 of
         (st, eqsEndo) -> let eqs = appEndo eqsEndo Map.empty
                              (iterations, solved) = solve eqs
                          in --trace (ppEquations eqs) $
                             --trace (ppEquations solved) $
                             (iterations, HeapAnalysis solved)


type M a = ReaderT HeapAnalysis (State Int) a

lower :: HeapAnalysis -> Grin -> Grin
lower hpt grin
    = evalState (runReaderT worker hpt) (grinUnique grin)
    where worker = do fns <- mapM lowerFuncDef (grinFunctions grin)
                      unique <- get
                      return grin{ grinFunctions = fns
                                 , grinUnique    = unique }

lowerFuncDef :: FuncDef -> M FuncDef
lowerFuncDef func
    = do body <- lowerExpression (funcDefBody func)
         return $ func{funcDefBody = body}

lowerExpression :: Expression -> M Expression
lowerExpression (a :>>= lam)
    = do a' <- lowerExpression a
         lam' <- lowerLambda lam
         return $ a' :>>= lam'
lowerExpression (Application (Builtin "eval") [a])
    = do f <- newVariable
         HeapAnalysis hpt <- ask
         case Map.lookup (VarEntry a) hpt of
           Just (Rhs rhs) -> do let Rhs rhs' = mconcat [ hpt Map.! HeapEntry hp | Heap hp <- rhs ]
                                alts <- mapM (mkApplyAlt []) rhs'
                                v <- newVariable
                                return $ Application (Builtin "fetch") [a] :>>= Variable f :->
                                         Case (Variable f) alts :>>= Variable v :->
                                         Application (Builtin "update") [a,v] :>>= Empty :->
                                         Unit (Variable v)
           Nothing -> return $ Application (Builtin "urk") []
lowerExpression (Application (Builtin "apply") [a,b])
    = do HeapAnalysis hpt <- ask
         case Map.lookup (VarEntry a) hpt of
           Just (Rhs rhs) -> do alts <- mapM (mkApplyAlt [b]) rhs
                                return $ Case (Variable a) alts
           Nothing -> return $ Application (Builtin "urk") []
lowerExpression (Application fn args)
    = return $ Application fn args
lowerExpression (Case scrut alts)
    = do alts' <- mapM lowerLambda alts
         return $ Case scrut alts'
lowerExpression (Store val)
    = return $ Store val
lowerExpression (Unit val) = return $ Unit val

lowerLambda :: Lambda -> M Lambda
lowerLambda (a :-> b)
    = do b' <- lowerExpression b
         return $ a :-> b'

mkApplyAlt extraArgs (Tag tag FunctionNode n argsRhs) | n == length extraArgs
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag FunctionNode n args :-> Application tag (args ++ extraArgs)
mkApplyAlt extraArgs (Tag tag nt n argsRhs)
    = do args <- replicateM (length argsRhs) newVariable
         return $ Node tag nt n args :-> Unit (Node tag nt (n - length extraArgs) (args ++ extraArgs))

newVariable :: M Renamed
newVariable = do unique <- get
                 put (unique + 1)
                 return $ Anonymous unique

