{-# LANGUAGE TemplateHaskell #-}
{-
SimpleCore is a subset of External Core that can more easily be translated to Grin.

We do
 * Hoist out local functions (lets and lambdas)

-}
module Grin.SimpleCore
  ( SimpleModule(..)
  , SimpleDef(..)
  , SimpleExp(..)
  , simpleDefArity
  , Alt(..)
  , Lit(..)
  , coreToSimpleCore
  ) where

import Grin.Types (Variable,NodeType(..),Value(..))
import Grin.SimpleCore.Types
import Language.Core (Ty,Vbind,Tdef,Vdef(..))
import qualified Language.Core as Core

import CompactString

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.RWS
import Data.List
import Data.Maybe

import Traverse

import Debug.Trace

{-
The data structures for the simplified core is similar to plain core with a few exceptions:
 * Lets always have the form 'let x = fn a b c in'
 * Lambdas have been removed
 * Application chains have been converted to lists of arguments.
-}







coreToSimpleCore :: Core.Module -> SimpleModule
coreToSimpleCore (Core.Module (pkgname,modname) tdefs vdefs)
    = let (_, simpleDefs) = execRWS (mapM_ vdefToSimpleDef allDefs) emptyScope 0
      in SimpleModule { modulePackage = L.unpack pkgname
                      , moduleName    = L.unpack modname
                      , moduleTypes   = tdefs
                      , moduleDefs    = simpleDefs
                      , moduleDeps    = coreDeps tdefs allDefs }
    where allDefs = concatMap (\x -> case x of Core.Nonrec d -> [d]; Core.Rec ds -> ds) vdefs
          emptyScope = Scope { currentScope  = Set.empty
                             , currentModule = (pkgname, modname) }


coreDeps tdefs vdefs
    = let free = Set.toList $ freeVariables (Core.Let (Core.Rec vdefs) (Core.Label "end"))
      in nub [ (L.unpack pkg, L.unpack mod) | (pkg,mod,_ident) <- free ]





--type ScopeEnv = Map.Map (Core.Qual Core.Id) Renamed
data Scope = Scope { currentScope :: Set.Set (Core.Qual Core.Id)
                   , currentModule :: (Core.Pkgname, Core.Mname) }
type M a = RWS Scope [SimpleDef] Int a

vdefToSimpleDef :: Core.Vdef -> M ()
vdefToSimpleDef vdef
    = let (args, body) = splitExp (vdefExp vdef)
      in bindVariables args $ vdefToSimpleDef' (qualToCompact (vdefName vdef)) [ qualToCompact var | var <- args ] body

vdefToSimpleDef' :: CompactString -> [CompactString] -> Core.Exp -> M ()
vdefToSimpleDef' name args body
    = do body' <- expToSimpleExp body
         tell [SimpleDef { simpleDefName = name
                         , simpleDefArgs = args
                         , simpleDefBody = body' }]

expToSimpleExp :: Core.Exp -> M SimpleExp
expToSimpleExp (Core.Var (pkg,mod,ident)) | pkg == L.pack "ghczmprim" && mod == L.pack "GHCziPrim"
    = return $ Primitive (qualToCompact (L.empty, L.empty, ident))
expToSimpleExp (Core.Var var)  = return $ Var (qualToCompact var)
expToSimpleExp (Core.Dcon con) = return $ Dcon (qualToCompact con)
expToSimpleExp (Core.Lit lit)  = return $ Lit $ fromCoreLit lit
expToSimpleExp (Core.App a b)  = return App `ap` expToSimpleExp a `ap` expToSimpleExp b
expToSimpleExp (Core.Appt a _) = expToSimpleExp a
expToSimpleExp (Core.Lam Core.Tb{} e) = expToSimpleExp e
-- We remove lambdas by translating them to let expressions.
expToSimpleExp exp@(Core.Lam (Core.Vb (var,ty)) _)
    = let def     = Vdef { vdefLocal = False
                         , vdefName  = var
                         , vdefType  = error "unknown type"
                         , vdefExp   = exp }
      in expToSimpleExp (Core.Let (Core.Nonrec def) (Core.Var (vdefName def)))
expToSimpleExp (Core.Let (Core.Nonrec def) e)
    = bindDef def $
      do (name, toplevelName, args, arity) <- lambdaLift def
         return (Let name toplevelName args arity) `ap` expToSimpleExp e
expToSimpleExp (Core.Let (Core.Rec defs) e)
    = bindDefs defs $ return LetRec `ap` mapM lambdaLift defs `ap` expToSimpleExp e
expToSimpleExp (Core.Case e bind ty alts)     = bindVariable (fst bind) $
                                                do e' <- expToSimpleExp e
                                                   alts' <- mapM altToSimpleAlt alts
                                                   let (mbDefault, alts'') = splitDefault alts'
                                                   return $ Case e' (qualToCompact $ fst bind) alts' Nothing -- mbDefault
expToSimpleExp (Core.Cast e _ty) = expToSimpleExp e
expToSimpleExp (Core.External target conv ty) = return $ External target conv
expToSimpleExp (Core.DynExternal conv ty)     = return $ DynExternal conv
expToSimpleExp (Core.Label label)             = return $ Label label
expToSimpleExp (Core.Note note e)             = return (Note note) `ap` expToSimpleExp e


altToSimpleAlt (Core.Acon con _tbinds vbinds e) = let bs = map fst vbinds
                                                  in return (Acon (qualToCompact con) (map qualToCompact bs)) `ap` bindVariables bs (expToSimpleExp e)
altToSimpleAlt (Core.Alit lit e)                = return (Alit $ fromCoreLit lit) `ap` expToSimpleExp e
altToSimpleAlt (Core.Adefault e)                = return Adefault `ap` expToSimpleExp e


collectApps (Core.App a b) = let (fn,args) = collectApps a
                        in (fn,args ++ [b])
collectApps e = (e,[])


splitDefault [] = (Nothing, [])
splitDefault (Adefault e:xs) = let (_,rest) = splitDefault xs
                               in (Just e, rest)
splitDefault (x:xs) = let (mbDefault,rest) = splitDefault xs
                      in (mbDefault, x:rest)


fromCoreLit (Core.Lint int _ty)           = Lint int
fromCoreLit (Core.Lrational rational _ty) = Lrational rational
fromCoreLit (Core.Lchar char _ty)         = Lchar char
fromCoreLit (Core.Lstring string _ty)     = Lstring string

{-
First we make the scope explicit:

let a = \x -> x + b in
->
let a b = \x -> x + b

Then we push the function to the top level:

top-level: let_a b = \x -> x + b
let a = let_a b

We have to do this because functions are represented with tags instead of
function pointers.
-}
lambdaLift :: Core.Vdef -> M (CompactString, CompactString, [Variable], Int)
lambdaLift vdef@Vdef{vdefName = (_pkg,_mod,ident), vdefExp = exp}
    = do (pkg,mod) <- asks currentModule
         scope <- asks currentScope
         let lambdaScope = Set.toList $ freeVariables exp `Set.intersection` scope
             (args,body) = splitExp exp
             realArgs = map qualToCompact (lambdaScope ++ args)
             toplevelName = qualToCompact (pkg,mod,L.pack "@lifted@_" `L.append` ident)

         bindVariables (lambdaScope ++ args) $ vdefToSimpleDef' toplevelName realArgs body

         return ( qualToCompact (vdefName vdef)
                , toplevelName
                , map qualToCompact lambdaScope
                , length realArgs )


noType = error "Urk, types shouldn't be needed"

freeVariables :: Core.Exp -> Set.Set (Core.Qual Core.Id)
freeVariables (Core.Var qual)                  = Set.singleton qual
freeVariables (Core.Dcon qual)                 = Set.singleton qual
freeVariables (Core.Lam (Core.Vb (var,_ty)) e) = Set.delete (var) $ freeVariables e
freeVariables (Core.Let (Core.Nonrec def) e)   = freeVariables (Core.Let (Core.Rec [def]) e)
freeVariables (Core.Let (Core.Rec defs) e)     = Set.unions (freeVariables e : (map (freeVariables . vdefExp) defs)) `Set.difference` bound
    where bound = Set.fromList (map vdefName defs)
freeVariables (Core.Case e (var,_ty) _ alts)
    = freeVariables e `Set.union` Set.delete (var) (Set.unions (map freeVariablesAlt alts))
freeVariables e = tsum freeVariables e

freeVariablesAlt (Core.Acon _con _tbinds vbinds e)
    = freeVariables e `Set.difference` Set.fromList [ (var) | (var, _ty) <- vbinds ]
freeVariablesAlt (Core.Alit _lit e)
    = freeVariables e
freeVariablesAlt (Core.Adefault e)
    = freeVariables e

{-
references :: Core.Exp -> Set.Set (Core.Qual Core.Id)
references (Core.Var qual)                  = Set.singleton qual
references (Core.Dcon qual)                 = Set.singleton qual
references (Core.Lam (Core.Vb (var,_ty)) e) = Set.delete (var) $ references e
references (Core.Let (Core.Nonrec def) e)   = references (Core.Let (Core.Rec [def]) e)
references (Core.Let (Core.Rec defs) e)     = Set.unions (references e : (map (references . vdefExp) defs)) `Set.difference` bound
    where bound = Set.fromList (map vdefName defs)
references (Core.Case e (var,_ty) _ alts)
    = references e `Set.union` Set.delete (var) (Set.unions (map referencesAlt alts))
references e = tsum references e

referencesAlt (Core.Acon _con _tbinds vbinds e)
    = references e `Set.difference` Set.fromList [ (var) | (var, _ty) <- vbinds ]
referencesAlt (Core.Alit _lit e)
    = references e
referencesAlt (Core.Adefault e)
    = referencesAlt e
-}



bindVariable :: Core.Qual Core.Id -> M a -> M a
bindVariable var
    = local $ \scope -> scope{ currentScope = Set.insert var (currentScope scope)}

bindVariables :: [Core.Qual Core.Id] -> M a -> M a
bindVariables [] = id
bindVariables (x:xs) = bindVariable x . bindVariables xs

bindDef :: Vdef -> M a -> M a
bindDef def
    = bindVariable (vdefName def)

bindDefs :: [Vdef] -> M a -> M a
bindDefs [] = id
bindDefs (x:xs) = bindDef x . bindDefs xs

{-
lookupType :: Variable -> M Ty
lookupType var = asks (Map.findWithDefault defaultVal var)
    where defaultVal = error $ "Grin.SimpleCore.lookupType: couldn't find type for: " ++ show var
-}

splitExp (Core.Lam (Core.Vb b) exp) = let (args,body) = splitExp exp
                                      in (fst b:args, body)
splitExp (Core.Lam _ exp) = splitExp exp
splitExp exp = ([], exp)









