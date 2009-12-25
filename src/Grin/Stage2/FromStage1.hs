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
          convertFuncs = do nodes <- funcDefsToNodes (Stage1.grinFunctions grin)
                            withNodeMap nodes $ do funcs <- mapM convertFuncDef (Stage1.grinFunctions grin)
                                                   cafs <- mapM convertCAF (Stage1.grinCAFs grin)
                                                   return (funcs, cafs, nodes)
      in case runRWS convertFuncs initReader initState of
           ((funcs, cafs, nodes), newUnique, stringCAFs)
             -> Grin { grinNodes     = Stage1.grinNodes grin ++
                                       [ NodeDef name FunctionNode []
                                       | names <- Map.elems nodes
                                       , name <- names ]
                     , grinCAFs      = cafs ++
                                       [ CAF { cafName = name, cafValue = Lit (Lstring string) }
                                         | (name, string) <- stringCAFs ]
                     , grinFunctions = funcs
                     , grinEntryPoint = Stage1.grinEntryPoint grin
                     , grinUnique    = newUnique
                     }

convertCAF :: Stage1.CAF -> M Stage2.CAF
convertCAF caf
    = do value <- case Stage1.cafValue caf of
                    Stage1.Node tag nt missing _args -> do tag' <- lookupTag tag missing
                                                           return $ Node tag' nt missing
                    other -> error $ "Grin.Stage2.FromStage1.convertCaf: Weird caf: " ++ show other
         return $ CAF { cafName  = Stage1.cafName caf
                      , cafValue = value }


type NodeMap = Map.Map Renamed [Renamed]
type M a = RWS (HeapAnalysis,NodeMap) [(Renamed,String)] Int a

funcDefToNode :: Stage1.FuncDef -> M (Renamed, [Renamed])
funcDefToNode def
    = do vars <- replicateM (arity+1) (newVariableFrom name)
         return (name, vars)
    where arity = length (Stage1.funcDefArgs def)
          name  = Stage1.funcDefName def

funcDefsToNodes :: [Stage1.FuncDef] -> M NodeMap
funcDefsToNodes defs
    = do nodes <- mapM funcDefToNode defs
         return $ Map.fromList nodes

withNodeMap :: NodeMap -> M a -> M a
withNodeMap nodes
    = local (\(hpt,nodes') -> (hpt, Map.union nodes nodes'))

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
    = convertFetch p
convertExpression (Stage1.Update size ptr val)
    = do [ptr'] <- lookupVariable ptr
         values <- fmap (take size) $ lookupVariable val
         if length values <= minNodeSize
            then return $ Application fn (ptr':values)
            else do extra <- newVariable
                    let (first,second) = splitAt (minNodeSize-1) values
                    return $ Store second :>>= [extra] :-> Application fn (ptr':first++[extra])
    where minNodeSize = 4
          fn = Builtin "update"
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
         vars <- replicateM (max 1 size) newVariable
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
           Other{rhsTagged = nodes} | not (Map.null nodes)
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
                   tag' <- lookupTag tag missing
                   return $ Node tag' nt missing :> fetches
              | otherwise
              = do argVars <- replicateM (length args) newVariable
                   extra <- newVariable
                   let (firstArgs, secondArgs) = splitAt (minNodeSize-2) argVars
                       firstFetches = foldr (\(v,n) r -> Stage2.Fetch n p :>>= [v] :-> r) secondFetches (zip firstArgs [1..])
                       secondFetches = Stage2.Fetch (minNodeSize-1) p :>>= [extra] :->
                                       foldr (\(v,n) r -> Stage2.Fetch n extra :>>= [v] :-> r) (Unit argVars) (zip secondArgs [0..])
                   tag' <- lookupTag tag missing
                   return $ Node tag' nt missing :> firstFetches
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
         return (lookupHeap val hpt)

find key m = Map.findWithDefault (error $ "Couldn't find key: " ++ show key) key m

newVariable :: M Renamed
newVariable = newVariableFrom (Builtin "newVariable.undefined")

newVariableFrom :: Renamed -> M Renamed
newVariableFrom original
    = do u <- get
         put (u+1)
         return $ merge original $ Anonymous u
    where merge (Aliased _ name) (Anonymous uid) = Aliased uid name
          merge _ renamed = renamed

convertAlt :: [Renamed] -> Stage1.Alt -> M Stage2.Alt
convertAlt vector (Stage1.Lit lit Stage1.:> alt)
    = do alt' <- convertExpression alt
         return $ Lit lit :> alt'
convertAlt vector (Stage1.Variable v Stage1.:> alt)
    = convertBind v $ \v' ->
      do alt' <- convertExpression alt
         return $ Stage2.Empty :> Unit vector :>>= v' :-> alt'
convertAlt vector (Stage1.Node tag FunctionNode missing args Stage1.:> alt)
    = do names <- lookupVariable tag
         alt' <- convertExpression alt
         return $ Node (names !! missing) FunctionNode missing :> Unit (tail vector) :>>= args :-> alt'
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
    = do tag' <- lookupTag tag missing
         v <- newVariable
         args' <- mapM lookupVariable args
         r <- fn (v:concat args')
         return $ Constant (Node tag' nt missing) :>>= [v] :-> r
convertValue fn (Stage1.Vector args)
    = do args' <- mapM lookupVariable args
         fn (concat args')

lookupVariable :: Renamed -> M [Renamed]
lookupVariable val
    = do nmap <- asks snd
         return $ Map.findWithDefault [val] val nmap

lookupTag :: Renamed -> Int -> M Renamed
lookupTag tag idx
    = do tags <- lookupVariable tag
         return (cycle tags !! idx)