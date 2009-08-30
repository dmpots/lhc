{-# LANGUAGE OverloadedStrings #-}
module Grin.Stage2.FromStage1
    ( convert
    ) where

import qualified Grin.Types as Stage1
import Grin.Stage2.Types as Stage2

import Grin.HPT

import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Monoid

convert :: HeapAnalysis -> Stage1.Grin -> Stage2.Grin
convert hpt grin
    = let initReader = (hpt,Map.empty)
          initState  = Stage1.grinUnique grin
          convertFuncs = mapM convertFuncDef (Stage1.grinFunctions grin)
      in case runRWS convertFuncs initReader initState of
           (funcs, newUnique, stringCAFs)
             -> Grin { grinNodes     = Stage1.grinNodes grin
                     , grinCAFs      = map convertCAF (Stage1.grinCAFs grin) ++
                                       [ CAF { cafName = name, cafValue = Lit (Lstring string) }
                                         | (name, string) <- stringCAFs ]
                     , grinFunctions = funcs
                     , grinEntryPoint = Stage1.grinEntryPoint grin
                     , grinUnique    = newUnique
                     }

convertCAF :: Stage1.CAF -> Stage2.CAF
convertCAF caf
    = CAF { cafName  = Stage1.cafName caf
          , cafValue = case Stage1.cafValue caf of
                         Stage1.Node tag nt missing _args -> Node tag nt missing
                         other -> error $ "Grin.Stage2.FromStage1.convertCaf: Weird caf: " ++ show other
          }


type NodeMap = Map.Map Renamed [Renamed]
type M a = RWS (HeapAnalysis,NodeMap) [(Renamed,String)] Int a

convertFuncDef :: Stage1.FuncDef -> M Stage2.FuncDef
convertFuncDef def
    = do body <- convertExpression (Stage1.funcDefBody def)
         returns <- nodeSize (Stage1.funcDefName def)
         return $ FuncDef { funcDefName = Stage1.funcDefName def
                          , funcDefReturns = returns
                          , funcDefArgs = Stage1.funcDefArgs def
                          , funcDefBody = body
                          }

convertExpression :: Stage1.Expression -> M Stage2.Expression
convertExpression (Stage1.Application (Builtin "unreachable") [] Stage1.:>>= _)
    = return $ Application (Builtin "unreachable") []
convertExpression (a Stage1.:>>= v Stage1.:-> b)
    = do a' <- convertExpression a
         convertBind v $ \v' ->
           do b' <- convertExpression b
              return $ a' :>>= v' :-> b'
convertExpression (a Stage1.:>> b)
    = do a' <- convertExpression a
         b' <- convertExpression b
         return $ a' :>>= [] :-> b'
convertExpression (Stage1.Application (Builtin "fetch") [p])
    = convertFetch p{-
    = do size <- heapNodeSize p
         [p'] <- lookupVariable p
         vars <- replicateM size newVariable
         return $ foldr (\(v,n) r -> Stage2.Fetch n p' :>>= [v] :-> r) (Unit vars) (zip vars [0..])-}
convertExpression (Stage1.Application fn@(Builtin "update") ~[ptr,val])
    = do [ptr'] <- lookupVariable ptr
         values <- lookupVariable val
         if length values <= minNodeSize
            then return $ Application fn (ptr':values)
            else do extra <- newVariable
                    let (first,second) = splitAt (minNodeSize-1) values
                    return $ Store second :>>= [extra] :-> Application fn (ptr':first++[extra])
    where minNodeSize = 4
convertExpression (Stage1.Application fn args)
    = do args' <- mapM lookupVariable args
         return $ Application fn (map head args')
convertExpression (Stage1.Case scrut alts)
    = do vector <- lookupVariable scrut
         case vector of
           [] -> return $ Application (Builtin "unreachable") []
           _  ->do alts'  <- mapM (convertAlt vector) alts
                   return $ Case (head vector) alts'
convertExpression (Stage1.Store (Stage1.Hole size))
    = return $ StoreHole size
convertExpression (Stage1.Store val)
    = convertValue worker val
    where worker args
              | length args <= minNodeSize
              = return (Store args)
              | otherwise
              = do let (firstArgs, secondArgs) = splitAt (minNodeSize-1) args
                   extra <- newVariable
                   return $ Store secondArgs :>>= [extra] :-> Store (firstArgs++[extra])
          minNodeSize = 4
convertExpression (Stage1.Unit val)
    = convertValue (return . Unit) val

convertBind :: Renamed -> ([Renamed] -> M a) -> M a
convertBind val fn
    = do size <- nodeSize val
         vars <- replicateM size newVariable
         local (\(hpt,nmap) -> (hpt,Map.insert val vars nmap)) $ fn vars



{-
do node <- fetch p
   node `elem` [ Nil, Cons x y, NearBig x y z, Big x y z n ]
===>
do tag <- fetch 0 p
   [x,y] <- case tag of
              Nil  -> do unit []
              Cons -> do x <- fetch 1 p
                         y <- fetch 2 p
                         unit [x,y]
              NearBig -> do x <- fetch 1 p
                            y <- fetch 2 p
                            z <- fetch 3 p
                            unit [x,y,z]
              Big  -> do x <- fetch 1 p
                         y <- fetch 2 p
                         extra <- fetch 3 p
                         z <- fetch 0 extra
                         n <- fetch 1 extra
                         unit [x,y,z,n,i]
-}
convertFetch p
    = do rhs <- heapNodeValues p
         case rhs of
           Tagged nodes
             -> do let size = rhsSize rhs
                   [p'] <- lookupVariable p
                   v <- newVariable
                   tmps <- replicateM (size-1) newVariable
                   vars <- replicateM size newVariable
                   alts <- mapM (mkAlt p') (Map.toList nodes)
                   return $ Stage2.Fetch 0 p' :>>= [v] :-> Case v alts :>>= tmps :-> Unit (v:tmps)
           Base
             -> do [p'] <- lookupVariable p
                   return (Stage2.Fetch 0 p')
           Heap _
             -> do [p'] <- lookupVariable p
                   return (Stage2.Fetch 0 p')
           _ -> do return (Application (Builtin "unreachable") [])
    where mkAlt p ((tag, nt, missing), args)
              | length args <= minNodeSize-1
              = do argVars <- replicateM (length args) newVariable
                   let fetches = foldr (\(v,n) r -> Stage2.Fetch n p :>>= [v] :-> r) (Unit (argVars)) (zip argVars [1..])
                   return $ Node tag nt missing :> fetches
              | otherwise
              = do argVars <- replicateM (length args) newVariable
                   extra <- newVariable
                   let (firstArgs, secondArgs) = splitAt (minNodeSize-2) argVars
                       firstFetches = foldr (\(v,n) r -> Stage2.Fetch n p :>>= [v] :-> r) secondFetches (zip firstArgs [1..])
                       secondFetches = Stage2.Fetch (minNodeSize-1) p :>>= [extra] :->
                                       foldr (\(v,n) r -> Stage2.Fetch n extra :>>= [v] :-> r) (Unit argVars) (zip secondArgs [0..])
                   return $ Node tag nt missing :> firstFetches
          -- FIXME: The node size should be configurable.
          minNodeSize = 4

nodeSize :: Renamed -> M Int
nodeSize val
    = do hpt <- asks fst
         return (rhsSize (lookupLhs (VarEntry val) hpt))

heapNodeSize :: Renamed -> M Int
heapNodeSize hp = do rhs <- heapNodeValues hp
                     return $ rhsSize rhs

heapNodeValues :: Renamed -> M Rhs
heapNodeValues val
    = do hpt <- asks fst
         return (lookupHeap (VarEntry val) hpt)

find key m = Map.findWithDefault (error $ "Couldn't find key: " ++ show key) key m

newVariable :: M Renamed
newVariable
    = do u <- get
         put (u+1)
         return $ Anonymous u


convertAlt :: [Renamed] -> Stage1.Alt -> M Stage2.Alt
convertAlt vector (Stage1.Lit lit Stage1.:> alt)
    = do alt' <- convertExpression alt
         return $ Lit lit :> alt'
convertAlt vector (Stage1.Variable v Stage1.:> alt)
    = convertBind v $ \v' ->
      do alt' <- convertExpression alt
         return $ Stage2.Empty :> Unit vector :>>= v' :-> alt'
convertAlt vector (Stage1.Node tag nt missing args Stage1.:> alt)
    = do alt' <- convertExpression alt
         return $ Node tag nt missing :> Unit (tail vector) :>>= args :-> alt'
convertAlt vector (Stage1.Vector args Stage1.:> alt)
    = do alt' <- convertExpression alt
         return $ Stage2.Empty :> Unit vector :>>= args :-> alt'
convertAlt vector (Stage1.Empty Stage1.:> alt)
    = error $ "Grin.Stage2.FromStage1.convertAlt: Empty case condition."
convertAlt vector (Stage1.Hole{} Stage1.:> alt)
    = error $ "Grin.Stage2.FromStage1.convertAlt: Invalid case condition."

convertValue :: ([Renamed] -> M Expression) -> Stage1.Value -> M Expression
convertValue fn (Stage1.Lit (Lstring string))
    = do v <- newVariable
         tell [(v,string)]
         fn [v]
convertValue fn (Stage1.Lit lit)    = do v <- newVariable
                                         r <- fn [v]
                                         return $ Constant (Lit lit) :>>= [v] :-> r
convertValue fn Stage1.Hole{}       = error "Grin.Stage2.FromStage1.convertValue: There shouldn't be a hole here."
convertValue fn (Stage1.Empty)      = fn []
convertValue fn (Stage1.Variable v) = fn =<< lookupVariable v
convertValue fn (Stage1.Node tag nt missing args)
    = do v <- newVariable
         args' <- mapM lookupVariable args
         r <- fn (v:concat args')
         return $ Constant (Node tag nt missing) :>>= [v] :-> r
convertValue fn (Stage1.Vector args)
    = do args' <- mapM lookupVariable args
         fn (concat args')

lookupVariable :: Renamed -> M [Renamed]
lookupVariable val
    = do nmap <- asks snd
         return $ Map.findWithDefault [val] val nmap

