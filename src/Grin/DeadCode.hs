module Grin.DeadCode where

import CompactString
import Grin.Types

import Debug.Trace
import qualified Data.Set as Set
import qualified Data.Map as Map



removeDeadCode :: [String] -> Grin -> Grin
removeDeadCode entryPoints grin
    = let entries = [ (nameMap Map.! fromString e) | e <- entryPoints ]
          loop seen ds = let deps = Set.filter isAliased $ Set.unions (map findFunc ds)
                             new  = deps `Set.difference` seen
                             seen' = Set.union seen deps
                         in if Set.null new then seen else loop seen' (Set.toList new)
          deps = Set.union (Set.fromList entries) (loop Set.empty entries)
      in grin { grinFunctions = [ def | def <- grinFunctions grin, funcDefName def `Set.member` deps]
              , grinNodes     = [ node | node <- grinNodes grin, nodeName node `Set.member` deps ]
              }
    where funcMap = Map.fromList [ (funcDefName def,defDependencies def) | def <- grinFunctions grin ]
                    `Map.union`
                    Map.fromList [ (nodeName node, Set.empty) | node <- grinNodes grin ]
          nameMap = Map.fromList [ (name, funcDefName def) | def@FuncDef{funcDefName = Aliased _ name} <- grinFunctions grin ]
          findFunc name = Map.findWithDefault (error $ "couldn't find function: " ++ show name) name funcMap

defDependencies :: FuncDef -> Set.Set Renamed
defDependencies def = dependencies (funcDefBody def) `Set.difference` Set.fromList (funcDefArgs def)

dependencies :: Expression -> Set.Set Renamed
dependencies (exp :>>= lam)
    = dependencies exp `Set.union` lambda lam
dependencies (Application fn args)
    = Set.insert fn $ Set.unions (map valueBound args)
dependencies (Case v alts)
    = Set.unions (valueBound v : map lambda alts)
dependencies (Fetch v)
    = error "Urk?"
dependencies (Store v)
    = valueBound v
dependencies (Unit v) = valueBound v

lambda (v :-> e) = dependencies e `Set.difference` valueBound v

valueBound (Node name _type args) = Set.insert name $ Set.unions (map valueBound args)
valueBound Lit{}                  = Set.empty
valueBound (Variable v)           = Set.singleton v
valueBound Hole{}                 = Set.empty
valueBound Empty                  = Set.empty

