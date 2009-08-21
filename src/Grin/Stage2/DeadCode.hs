{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}
module Grin.Stage2.DeadCode
    ( trimDeadCode
    , calcLiveNodes
    ) where

import Grin.Stage2.Types

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Debug.Trace

calcLiveNodes :: Grin -> IO ()
calcLiveNodes grin
    = do let live = liveNodes grin
         writeFile "livenodes.txt" (unlines (map show (IntSet.toList live)))

trimDeadCode :: Grin -> Grin
trimDeadCode grin
    = grin { grinFunctions = map walkFunc [ fn | fn <- grinFunctions grin, nodeId (funcDefName fn) `IntSet.member` liveSet]
           , grinCAFs      = [ caf | caf <- grinCAFs grin, nodeId (cafName caf) `IntSet.member` liveSet ]
           , grinNodes     = [ node | node <- grinNodes grin, nodeId (nodeName node) `IntSet.member` liveSet || True ]
           }
    where walkFunc func
              = func { funcDefBody = walkExp (funcDefBody func) }
          walkExp (Store{} :>>= [bind] :-> e2) | nodeId bind `IntSet.notMember` liveSet
              = Unit [] :>>= [] :-> walkExp e2
          walkExp (StoreHole{} :>>= [bind] :-> e2) | nodeId bind `IntSet.notMember` liveSet
              = Unit [] :>>= [] :-> walkExp e2
          walkExp (Fetch{} :>>= [bind] :-> e2) | nodeId bind `IntSet.notMember` liveSet
              = Unit [] :>>= [] :-> walkExp e2
          walkExp (Constant{} :>>= [bind] :-> e2) | nodeId bind `IntSet.notMember` liveSet
              = Unit [] :>>= [] :-> walkExp e2
          walkExp (Application fn args :>>= binds :-> e2) | nodeId fn `IntSet.notMember` liveSet
              = Unit [] :>>= [] :-> walkExp e2
          walkExp (Application fn args :>>= [] :-> e2) | not (isBuiltin fn)
              = Unit [] :>>= [] :-> walkExp e2
          walkExp (e1 :>>= binds :-> e2)
              = if or [ nodeId bind `IntSet.member` liveSet | bind <- binds ] || null binds || True
                then walkExp e1 :>>= binds :-> walkExp e2
                else walkExp e2
          walkExp (Case scrut alts)
              = if nodeId scrut `IntSet.member` liveSet || True
                then Case scrut (map walkAlt alts)
                else Unit []
          walkExp fn@(Application (Builtin "update") (ptr:_))
              | nodeId ptr `IntSet.member` liveSet
              = fn
              | otherwise
              = Unit []
          walkExp fn = fn
          walkAlt (alt :> exp) = alt :> walkExp exp
          liveSet = liveNodes grin

liveNodes :: Grin -> IntSet.IntSet
liveNodes grin
    = let entryPoint = nodeId (grinEntryPoint grin)
          graph = execSM (grinGraph grin) entryPoint IntMap.empty
      in reachable entryPoint graph

reachable :: Int -> DependencyGraph -> IntSet.IntSet
reachable entry graph
    = loop (IntSet.singleton entry) (IntSet.singleton entry)
    where loop marked new | IntSet.null new = marked
          loop marked new
              = let reachableByNew = IntSet.unions [ find node | node <- IntSet.toList new ]
                    unmarkedNew = reachableByNew `IntSet.difference` marked
                in loop (marked `IntSet.union` unmarkedNew) unmarkedNew
          find key = IntMap.findWithDefault IntSet.empty key graph



newtype SM a = SM { runSM :: Int -> DependencyGraph -> (a, DependencyGraph) }

instance Monad SM where
    return x = SM $ \r s -> (x, s)
    f >>= g  = SM $ \r s -> case runSM f r s of
                              (a, !s') -> runSM (g a) r s'

instance MonadState (IntMap.IntMap IntSet.IntSet) SM where
    get = SM $ \_ s -> (s, s)
    put s = SM $ \_ _ -> ((), s)

instance MonadReader Int SM where
    ask = SM $ \r s -> (r, s)
    local fn m = SM $ \r s -> runSM m (fn r) s

execSM action r s
    = case runSM action r s of
        (a, s) -> s

type DependencyGraph = IntMap.IntMap IntSet.IntSet

type M a = SM a

top :: M Int
top = ask

grinGraph :: Grin -> M ()
grinGraph grin
    = do mapM_ cafGraph (grinCAFs grin)
         mapM_ funcGraph (grinFunctions grin)

insert k v m = let v' = IntMap.findWithDefault IntSet.empty k m
               in IntMap.insertWith IntSet.union k v m

cafGraph :: CAF -> M ()
cafGraph caf
    = do deps <- valueGraph (cafValue caf)
         modify $ insert (nodeId (cafName caf)) deps
         return ()

funcGraph :: FuncDef -> M ()
funcGraph func
    = do bodyDeps <- local (const (nodeId (funcDefName func))) $ expGraph (funcDefBody func)
         modify $ insert (nodeId (funcDefName func)) bodyDeps
         return ()

expGraph :: Expression -> M IntSet.IntSet
expGraph (Unit vals)
    = return $ IntSet.fromList (map nodeId vals)
expGraph (e1 :>>= binds :-> e2)
    = do deps <- expGraph e1
         forM_ binds $ \bind -> modify $ insert (nodeId bind) deps
         expGraph e2
expGraph (Application (Builtin "updateMutVar") [ptr, val, realWorld])
    = do --modify $ insert (nodeId ptr) (IntSet.singleton (nodeId val))
         --modify $ insert (nodeId realWorld) (IntSet.singleton (nodeId ptr))
         return $ IntSet.fromList [nodeId realWorld, nodeId ptr, nodeId val]
expGraph (Application (Builtin "update") (ptr:vals))
    = do t <- top
         let s = IntSet.fromList (map nodeId vals)
         modify $ insert (nodeId ptr) s
         return IntSet.empty
expGraph (Application fn args)
    = return $ IntSet.fromList (map nodeId (fn:args))
expGraph (Case scrut alts)
    = do depss <- mapM altGraph alts
         forM_ depss $ \deps ->
          do modify $ insert (nodeId scrut) deps
             forM_ (IntSet.toList deps) $ \dep ->
               modify $ insert dep (IntSet.singleton (nodeId scrut))
         return $ IntSet.singleton (nodeId scrut)
expGraph (Fetch _idx hp)
    = return $ IntSet.singleton (nodeId hp)
expGraph (Store vals)
    = return $ IntSet.fromList (map nodeId vals)
expGraph (StoreHole _size)
    = return IntSet.empty
expGraph (Constant value)
    = valueGraph value

nodeId :: Renamed -> Int
nodeId (Aliased uid _name) = uid
nodeId (Anonymous uid) = uid
nodeId (Builtin{}) = -1
nodeId (External{}) = -1

altGraph :: Alt -> M IntSet.IntSet
altGraph (value :> exp)
    = liftM2 (IntSet.union) (valueGraph value) (expGraph exp)

valueGraph :: Value -> M IntSet.IntSet
valueGraph (Node tag _nt _partial) = return $ IntSet.singleton (nodeId tag)
valueGraph Lit{} = return IntSet.empty
valueGraph Hole = return IntSet.empty
valueGraph Empty = return IntSet.empty

{-
liveNodes :: Grin -> [Node]
liveNodes grin
    = let graph = appREndo (execWriter (grinGraph grin)) empty
      in reachable (nodeId (grinEntryPoint grin)) graph

type DependencyGraph = Gr () ()

type M a = Writer (REndo DependencyGraph) a

newtype REndo a = REndo { appREndo :: (a -> a) }
instance Monoid (REndo a) where
    mempty = REndo id
    mappend (REndo f) (REndo g) = REndo (g . f)

defineNode :: Renamed -> M ()
defineNode node
    = tell $ REndo $ insNode (nodeId node, ())


grinGraph :: Grin -> M ()
grinGraph grin
    = do mapM_ (defineNode . cafName) (grinCAFs grin)
         mapM_ (defineNode . funcDefName) (grinFunctions grin)
         mapM_ (defineNode . nodeName) (grinNodes grin)
         tell $ REndo $ insNode (-1, ())
         mapM_ cafGraph (grinCAFs grin)
         mapM_ funcGraph (grinFunctions grin)

cafGraph :: CAF -> M [Node]
cafGraph caf
    = do deps <- valueGraph (cafValue caf)
         tell $ REndo $ insEdges [ (nodeId (cafName caf), dep, ()) | dep <-deps ]
         return [ nodeId (cafName caf) ]

funcGraph :: FuncDef -> M [Node]
funcGraph func
    = do mapM_ defineNode (funcDefArgs func)
         bodyDeps <- expGraph (funcDefBody func)
         tell $ REndo $ insEdges [ (nodeId (funcDefName func), dep, ()) | dep <- bodyDeps ]
         return [ nodeId (funcDefName func) ]

expGraph :: Expression -> M [Node]
expGraph (Unit vals)
    = return (map nodeId vals)
expGraph (e1 :>>= binds :-> e2)
    = do mapM_ defineNode binds
         deps <- expGraph e1
         tell $ REndo $ insEdges [ (nodeId bind, dep, ()) | bind <- binds, dep <- deps ]
         expGraph e2
expGraph (Application (Builtin "update") (ptr:vals))
    = do tell $ REndo $ insEdges [ (nodeId ptr, nodeId val,()) | val <- vals ]
         return (map nodeId (ptr:vals))
expGraph (Application fn args)
    = return (map nodeId (fn:args))
expGraph (Case scrut alts)
    = do deps <- mapM altGraph alts
         tell $ REndo $ insEdges [ (nodeId scrut, dep, ()) | dep <- concat deps ]
         tell $ REndo $ insEdges [ (dep, nodeId scrut, ()) | dep <- concat deps ]
         return [nodeId scrut]
expGraph (Fetch _idx hp)
    = return [nodeId hp]
expGraph (Store vals)
    = return (map nodeId vals)
expGraph (StoreHole _size)
    = return []
expGraph (Constant value)
    = valueGraph value

nodeId :: Renamed -> Int
nodeId (Aliased uid _name) = uid
nodeId (Anonymous uid) = uid
nodeId (Builtin{}) = -1
nodeId (External{}) = -1

altGraph :: Alt -> M [Node]
altGraph (value :> exp)
    = liftM2 (++) (valueGraph value) (expGraph exp)

valueGraph :: Value -> M [Node]
valueGraph (Node tag _nt _partial) = return [nodeId tag]
valueGraph Lit{} = return []
valueGraph Hole = return []
valueGraph Empty = return []
-}
