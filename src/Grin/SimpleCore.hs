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

import Grin.Types (Variable)
import Grin.SimpleCore.Types
import Language.Core (Ty,Tdef,Vdef(..))
import qualified Language.Core as Core

import CompactString

import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.RWS
import Data.List
import Data.Maybe

import Traverse

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


coreDeps :: [Tdef] -> [Vdef] -> [(String, String)]
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
expToSimpleExp (Core.Lamt _ e) = expToSimpleExp e
-- We remove lambdas by translating them to let expressions.
expToSimpleExp exp@(Core.Lam (var,ty) _)
    = do newVar <- uniqueQual var
         let def     = Vdef { vdefLocal = False
                            , vdefName  = newVar
                            , vdefType  = Core.Tvar $ error "unknown type" -- Urk.
                            , vdefExp   = exp }
         expToSimpleExp (Core.Let (Core.Nonrec def) (Core.Var newVar))
expToSimpleExp (Core.Let (Core.Nonrec def) e) | defIsStrictPrimitive def
    = bindVariable (vdefName def) $
      return (LetStrict (qualToCompact (vdefName def))) `ap` expToSimpleExp (vdefExp def) `ap` expToSimpleExp e
expToSimpleExp (Core.Let (Core.Nonrec def) e)
    = bindDef def $
      do (name, toplevelName, args, arity) <- lambdaLift def
         return (Let name toplevelName args arity) `ap` expToSimpleExp e
expToSimpleExp (Core.Let (Core.Rec defs) e)
    = bindDefs defs $ return LetRec `ap` mapM lambdaLift defs `ap` expToSimpleExp e
expToSimpleExp (Core.Case e bind ty alts)     = bindVariable (fst bind) $
                                                do e' <- expToSimpleExp e
                                                   alts' <- mapM altToSimpleAlt alts
                                                   return $ Case e' (qualToCompact $ fst bind) alts' Nothing
expToSimpleExp (Core.Cast e _ty) = expToSimpleExp e
expToSimpleExp (Core.External target conv ty) = return $ External target conv
expToSimpleExp (Core.DynExternal conv ty)     = return $ DynExternal conv
expToSimpleExp (Core.Label label)             = return $ Label label
expToSimpleExp (Core.Note note e)             = {- return (Note note) `ap` -} expToSimpleExp e


-- FIXME: This function is incomplete.
defIsStrictPrimitive :: Vdef -> Bool
defIsStrictPrimitive def
    = case vdefType def of
        Core.Tcon (pkg, mod, _ident) -> pkg == L.pack "ghczmprim" && mod == L.pack "GHCziPrim"
        _ -> False

altToSimpleAlt :: Core.Alt -> M Alt
altToSimpleAlt (Core.Acon con _tbinds vbinds e) = let bs = map fst vbinds
                                                  in return (Acon (qualToCompact con) (map qualToCompact bs)) `ap` bindVariables bs (expToSimpleExp e)
altToSimpleAlt (Core.Alit lit e)                = return (Alit $ fromCoreLit lit) `ap` expToSimpleExp e
altToSimpleAlt (Core.Adefault e)                = return Adefault `ap` expToSimpleExp e




fromCoreLit :: Core.Lit -> Lit
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
         unique <- newUnique
         let allFreeVars = freeVariables exp `Set.intersection` scope
             isRecursive = vdefName vdef `Set.member` allFreeVars
             lambdaScope = Set.toList $ if isCAF then allFreeVars else Set.delete (vdefName vdef) allFreeVars
             (args,body) = splitExp exp
             isCAF = null args
             realArgs = map qualToCompact (lambdaScope ++ args)
             toplevelName = (pkg,mod,L.pack "@lifted@_" `L.append` ident `L.append` L.pack (show unique))
             selfDef = Core.Case (foldl Core.App (Core.Var toplevelName) (map Core.Var lambdaScope))
                                 (vdefName vdef,vdefType vdef)
                                 (vdefType vdef)
                                 [Core.Adefault body]

         bindVariables (lambdaScope ++ args) $
           if isCAF || not isRecursive
             then vdefToSimpleDef' (qualToCompact toplevelName) realArgs body
             else vdefToSimpleDef' (qualToCompact toplevelName) realArgs selfDef

         return ( qualToCompact (vdefName vdef)
                , qualToCompact toplevelName
                , map qualToCompact lambdaScope
                , length realArgs )


noType :: Core.Ty
noType = error "Urk, types shouldn't be needed"

freeVariables :: Core.Exp -> Set.Set (Core.Qual Core.Id)
freeVariables (Core.Var qual)                  = Set.singleton qual
freeVariables (Core.Dcon qual)                 = Set.singleton qual
freeVariables (Core.Lam (var,_ty) e)           = Set.delete (var) $ freeVariables e
freeVariables (Core.Let (Core.Nonrec def) e)   = freeVariables (Core.Let (Core.Rec [def]) e)
freeVariables (Core.Let (Core.Rec defs) e)     = Set.unions (freeVariables e : (map (freeVariables . vdefExp) defs)) `Set.difference` bound
    where bound = Set.fromList (map vdefName defs)
freeVariables (Core.Case e (var,_ty) _ alts)
    = freeVariables e `Set.union` Set.delete (var) (Set.unions (map freeVariablesAlt alts))
freeVariables e = tsum freeVariables e

freeVariablesAlt :: Core.Alt -> Set.Set (Core.Qual Core.Id)
freeVariablesAlt (Core.Acon _con _tbinds vbinds e)
    = freeVariables e `Set.difference` Set.fromList [ (var) | (var, _ty) <- vbinds ]
freeVariablesAlt (Core.Alit _lit e)
    = freeVariables e
freeVariablesAlt (Core.Adefault e)
    = freeVariables e




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

newUnique :: M Int
newUnique = do u <- get
               put $! u+1
               return u

uniqueQual :: Core.Qual Core.Id -> M (Core.Qual Core.Id)
uniqueQual (pkg,mod,ident)
    = do u <- newUnique
         return (pkg, mod, ident `L.append` L.pack (show u))


splitExp :: Core.Exp -> ([Core.Qual Core.Id], Core.Exp)
splitExp (Core.Lam b exp) = let (args,body) = splitExp exp
                                in (fst b:args, body)
splitExp (Core.Lamt _ exp) = splitExp exp
splitExp (Core.Note _ exp) = splitExp exp
splitExp exp = ([], exp)









