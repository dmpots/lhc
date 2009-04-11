module Grin.DeadCode
    ( removeDeadCode
    ) where

import CompactString
import Grin.Types

import qualified Data.Set as Set
import qualified Data.Map as Map



removeDeadCode :: [String] -> Grin -> Grin
removeDeadCode entryPoints grin
    = let entries = [ (Map.findWithDefault (error $ "Couldn't find entry point: " ++ e) (fromString e) nameMap) | e <- entryPoints ]
          loop seen ds = let deps = Set.filter isAliased $ Set.unions (map findFunc ds)
                             new  = deps `Set.difference` seen
                             seen' = Set.union seen deps
                         in if Set.null new then seen else loop seen' (Set.toList new)
          deps = Set.union (Set.fromList entries) (loop Set.empty entries)
      in grin { grinFunctions = [ def  | def  <- grinFunctions grin, funcDefName def `Set.member` deps]
              , grinNodes     = [ node | node <- grinNodes grin, nodeName node `Set.member` deps ]
              , grinCAFs      = [ caf  | caf  <- grinCAFs grin, cafName caf `Set.member` deps ]
              }
    where funcMap = Map.fromList [ (funcDefName def,defDependencies def) | def <- grinFunctions grin ]
                    `Map.union`
                    Map.fromList [ (nodeName node, Set.empty) | node <- grinNodes grin ]
                    `Map.union`
                    Map.fromList [ (cafName caf, valueDependencies (cafValue caf)) | caf <- grinCAFs grin ]
          nameMap = Map.fromList [ (name, funcDefName def) | def@FuncDef{funcDefName = Aliased _ name} <- grinFunctions grin ]
                    `Map.union`
                    Map.fromList [ (name, cafName caf) | caf@CAF{cafName = Aliased _ name} <- grinCAFs grin]
          findFunc name = Map.findWithDefault (error $ "couldn't find function: " ++ show name) name funcMap


defDependencies :: FuncDef -> Set.Set Renamed
defDependencies def = dependencies (funcDefBody def) `Set.difference` Set.fromList (funcDefArgs def)

dependencies :: Expression -> Set.Set Renamed
dependencies (exp :>>= lam)
    = dependencies exp `Set.union` lambda lam
dependencies (Application fn args)
    = Set.insert fn $ Set.unions (map valueDependencies args)
dependencies (Case v alts)
    = Set.unions (valueDependencies v : map lambda alts)
dependencies (Store v)
    = valueDependencies v
dependencies (Unit v) = valueDependencies v

lambda :: Lambda -> Set.Set Renamed
lambda (v :-> e) = (valueDependencies v `Set.union` dependencies e) `Set.difference` valueBound v

valueBound :: Value -> Set.Set Renamed
valueBound (Node name _type args) = Set.unions (map valueBound args)
valueBound Lit{}                  = Set.empty
valueBound (Variable v)           = Set.singleton v
valueBound Hole{}                 = Set.empty
valueBound Empty                  = Set.empty

valueDependencies :: Value -> Set.Set Renamed
valueDependencies (Node name _type args) = Set.insert name $ Set.unions (map valueDependencies args)
valueDependencies (Variable v) = Set.singleton v
valueDependencies _ = Set.empty
