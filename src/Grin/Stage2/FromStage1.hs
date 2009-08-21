{-# LANGUAGE OverloadedStrings #-}
module Grin.Stage2.FromStage1
    ( convert
    ) where

import qualified Grin.Types as Stage1
import Grin.Stage2.Types as Stage2

import Grin.HPT.Environment
import Grin.HPT.Solve
--import Grin.HPT.QuickSolve
--import Grin.HPT.Environment (Lhs(..))

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
convertExpression (Stage1.Application fn@(Builtin "update") args)
    = do args' <- mapM lookupVariable args
         return $ Application fn (concat args')
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
    = convertValue Store val
convertExpression (Stage1.Unit val)
    = convertValue Unit val

convertBind :: Renamed -> ([Renamed] -> M a) -> M a
convertBind val fn
    = do size <- nodeSize val
         vars <- replicateM size newVariable
         local (\(hpt,nmap) -> (hpt,Map.insert val vars nmap)) $ fn vars



{-
do node <- fetch p
   node `elem` [ Nil, Cons x y ]
===>
do tag <- fetch 0 p
   [x,y] <- case tag of
              Nil  -> do unit []
              Cons -> do x <- fetch 1 p
                         y <- fetch 2 p
                         unit [x,y]
-}
convertFetch p
    = do values <- heapNodeValues p
         let taggedValues = filter isInteresting values
             isInteresting Tag{} = True
             isInteresting Indirection = True
             isInteresting _ = False
             size = maximum (0:map rhsValueSize taggedValues)
         [p'] <- lookupVariable p
         if null taggedValues
            then if not (null values)
                 then return (Stage2.Fetch 0 p')
                 else return (Application (Builtin "unreachable") [])
            else do v <- newVariable
                    tmps <- replicateM (size-1) newVariable
                    vars <- replicateM size newVariable
                    alts <- mapM (mkAlt p') taggedValues
                    return $ Stage2.Fetch 0 p' :>>= [v] :-> Case v alts :>>= tmps :-> Unit (v:tmps)
    where mkAlt p (Tag tag nt missing args)
              = do argVars <- replicateM (length args) newVariable
                   let fetches = foldr (\(v,n) r -> Stage2.Fetch n p :>>= [v] :-> r) (Unit (argVars)) (zip argVars [1..])
                   return $ Node tag nt missing :> fetches
          mkAlt p Indirection
              = do var <- newVariable
                   let indirection = Aliased (-1) "Indirection"
                       fetch = Stage2.Fetch 1 p :>>= [var] :-> Unit [var]
                   return $ Node indirection ConstructorNode 0 :> fetch

nodeSize :: Renamed -> M Int
nodeSize val
    = do HeapAnalysis hpt <- asks fst
         let Rhs vals = Map.findWithDefault mempty (VarEntry val) hpt
--         let Data vals = Map.findWithDefault mempty (VarEntry val) hpt
         return $ maximum (0:map rhsValueSize vals)

heapNodeSize :: Renamed -> M Int
heapNodeSize hp = do values <- heapNodeValues hp
                     return $ maximum (0:map rhsValueSize values)

heapNodeValues :: Renamed -> M [RhsValue]
heapNodeValues val
    = do HeapAnalysis hpt <- asks fst
         let Rhs vals = find (VarEntry val) hpt
--         let Data vals = find (VarEntry val) hpt
             hps      = map (\(Heap hp) -> hp) vals
             Rhs allVals  = mconcat [ vals | hp <- hps, let vals = find (HeapEntry hp) hpt ]
--               allVals  = [ val | hp <- hps, let Data vals = find (HeapEntry hp) hpt, val <- vals ]
         return allVals
-- $ maximum (0:map rhsValueSize allVals)

rhsValueSize (Base) = 1
rhsValueSize (Tag _tag _nt _missing args) = 1 + length args
rhsValueSize (VectorTag args) = length args
rhsValueSize (Heap{}) = 1
rhsValueSize Indirection = 2
rhsValueSize v = error $ "Grin.Stage2.FromStage1.nodeSize: Invalid rhs value: " ++ show v

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
         return $ Empty :> Unit vector :>>= v' :-> alt'
convertAlt vector (Stage1.Node tag nt missing args Stage1.:> alt)
    = do alt' <- convertExpression alt
         return $ Node tag nt missing :> Unit (tail vector) :>>= args :-> alt'
convertAlt vector (Stage1.Vector args Stage1.:> alt)
    = do alt' <- convertExpression alt
         return $ Empty :> Unit vector :>>= args :-> alt'
convertAlt vector (Stage1.Empty Stage1.:> alt)
    = error $ "Grin.Stage2.FromStage1.convertAlt: Empty case condition."
convertAlt vector (Stage1.Hole{} Stage1.:> alt)
    = error $ "Grin.Stage2.FromStage1.convertAlt: Invalid case condition."

convertValue :: ([Renamed] -> Expression) -> Stage1.Value -> M Expression
convertValue fn (Stage1.Lit (Lstring string))
    = do v <- newVariable
         tell [(v,string)]
         return $ fn [v]
convertValue fn (Stage1.Lit lit)    = do v <- newVariable
                                         return $ Constant (Lit lit) :>>= [v] :-> fn [v]
convertValue fn Stage1.Hole{}       = error "Grin.Stage2.FromStage1.convertValue: There shouldn't be a hole here."
convertValue fn (Stage1.Empty)      = return $ fn []
convertValue fn (Stage1.Variable v) = liftM fn (lookupVariable v)
convertValue fn (Stage1.Node tag nt missing args)
    = do v <- newVariable
         args' <- mapM lookupVariable args
         return $ Constant (Node tag nt missing) :>>= [v] :-> fn (v:concat args')
convertValue fn (Stage1.Vector args)
    = do args' <- mapM lookupVariable args
         return $ fn (concat args')

lookupVariable :: Renamed -> M [Renamed]
lookupVariable val
    = do nmap <- asks snd
         return $ Map.findWithDefault [val] val nmap

