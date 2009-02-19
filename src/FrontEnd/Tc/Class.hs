-- | Typeclass-related code, still heavily based on /Typing Haskell in Haskell/
--   <http://web.cecs.pdx.edu/~mpj/thih/>.

module FrontEnd.Tc.Class(
    Pred,
    ClassHierarchy(),
    splitPreds,
    generalize,
    splitReduce,
    topDefaults,
    freeMetaVarsPreds,
    simplify,
    assertEntailment,
    assertEquivalant,
    Preds
    )where

import Control.Monad.Trans
import Data.Monoid
import List
import Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import Doc.DocLike
import Doc.PPrint
import Doc.Pretty (Doc)
import FrontEnd.Diagnostic
import FrontEnd.Class
import FrontEnd.Tc.Monad
import FrontEnd.Tc.Type
import FrontEnd.Tc.Kind
import Name.Names
import Name.Name
import Options
import Support.CanType
import FrontEnd.Warning
import qualified FlagDump as FD
import qualified FlagOpts as FO
import Debug.Trace

render :: Doc -> String
render = show


generalize :: [Pred] -> Rho -> Tc Sigma
generalize ps r = do
    ch <- getClassHierarchy
    r <- flattenType r
    fmvenv <- freeMetaVarsEnv
    let mvs =  freeMetaVars r `Set.difference` fmvenv
    --(nps,rp) <- splitPreds ch (Set.toList fmvenv) ps
    (mvs',nps,rp) <- splitReduce fmvenv mvs (simplify ch ps)
    addPreds nps
    quantify mvs' rp r

-- | Unsurprisingly, this finds the free 'MetaVar's of some 'Preds'.
freeMetaVarsPreds :: Preds -> Set.Set MetaVar
freeMetaVarsPreds ps = Set.unions (map freeMetaVarsPred ps)

-- | Yes, this finds the free 'MetaVar's of a 'Pred'.
freeMetaVarsPred :: Pred -> Set.Set MetaVar
freeMetaVarsPred (IsIn _ t) = freeMetaVars t
freeMetaVarsPred (IsEq t1 t2) = freeMetaVars t1 `Set.union` freeMetaVars t2

-- | Split predicates into ones that only mention metavars in the list vs other ones,
--   after getting rid of the silly and redundant ones. (Based on THIH's @split@).
splitPreds :: Monad m
           => ClassHierarchy
           -> Set.Set MetaVar -- ^ \"fixed\" metavars
           -> Preds
           -> m (Preds, Preds)
splitPreds h fs ps  = do
    ps' <- toHnfs h ps
    return $ partition (\p -> freeMetaVarsPred p `Set.isSubsetOf` fs) $ simplify h  $ ps'

-- | From THIH. Convert many predicates to 'head-normal form'.
toHnfs      :: Monad m => ClassHierarchy -> [Pred] -> m [Pred]
toHnfs h ps =  mapM (toHnf h) ps >>= return . concat

-- | From THIH. Convert a predicate to as many 'head-normal form' predicates as needed to replace it.
toHnf :: Monad m => ClassHierarchy -> Pred -> m [Pred]
toHnf h p
    | inHnf p = return [p]
    | otherwise =  case reducePred h p of
         Nothing -> fail $ "context reduction, no instance for: "  ++ (pprint  p)
         Just ps -> toHnfs h ps

-- | Extended from THIH. Check whether or not the given Pred is in 'head-normal form'.
inHnf       :: Pred -> Bool
inHnf (IsEq t1 t2) = True
inHnf (IsIn c t) = hnf t
 where hnf (TVar v)  = True
       hnf TMetaVar {} = True
       hnf (TCon tc) = False
       hnf (TAp t _) = hnf t
       hnf (TArrow _t1 _t2) = False
       hnf TForAll {} = False
       hnf TExists {} = False
       hnf TAssoc {} = True


reducePred :: Monad m => ClassHierarchy -> Pred -> m [Pred]
reducePred h p@(IsEq t1 t2) = fail "reducePred" -- return [p]
reducePred h p@(IsIn c t)
    | Just x <- foldr mplus Nothing poss = return x
    | otherwise = fail "reducePred"
 where poss = map (byInst p) (instsOf h c)


simplify :: ClassHierarchy -> [Pred] -> [Pred]
simplify h ps = loop [] ps where
    loop rs []     = rs
    loop rs (p:ps)
        | entails h (rs ++ ps) p = loop rs ps
        | otherwise = loop (p:rs) ps


-- | Returns true when set of predicates implies some other predicate is satisfied.
entails :: ClassHierarchy -> [Pred] -> Pred -> Bool
--entails h ps e@(IsEq {}) = error $ pprint (ps,e)
entails h ps p = (p `elem` concatMap (bySuper h) ps) ||
           case reducePred h p of
             Nothing -> False
             Just qs -> all (entails h ps) qs

-- | From THIH. Returns a list of all predicates implied by the given predicate due to superclasses
bySuper :: ClassHierarchy -> Pred -> [Pred]
bySuper h p@IsEq {} = [p]
bySuper h p@(IsIn c t)
 = p : concatMap (bySuper h) supers
   where supers = [ IsIn c' t | c' <- supersOf h c ]

-- | Based on THIH, but the 'Inst' argument is new. If the given
--   instance could be used to satisfy the given predicate, return a
--   list of subgoals that must also be satisfied.
byInst             :: Monad m => Pred -> Inst -> m [Pred]
byInst p Inst { instHead = ps :=> h } = do
    u <- matchPred h p
    return (map (inst mempty (Map.fromList [ (tyvarAtom mv,t) | (mv,t) <- u ])) ps)

-- | Devise a substitution to convert the first argument to the second using 'match'.
matchPred :: Monad m => Pred -> Pred -> m [(Tyvar,Type)]
matchPred x@(IsIn c t) y@(IsIn c' t')
      | c == c'   = match t t'
matchPred x y = fail $ "Classes do not match: " ++ show (x,y)

supersOf :: ClassHierarchy -> Class -> [Class]
supersOf ch c = asksClassRecord ch c classSupers
instsOf :: ClassHierarchy -> Class -> [Inst]
instsOf ch c = asksClassRecord ch c classInsts


match :: Monad m => Type -> Type -> m [(Tyvar,Type)]
match x y = do match' x y
match' :: Monad m => Type -> Type -> m [(Tyvar,Type)]
match' (TAp l r) (TAp l' r') = do
    sl <- match l l'
    sr <- match r r'
    return $ mappend sl sr
match' (TArrow l r) (TArrow l' r') = do
    sl <- match l l'
    sr <- match r r'
    return $ mappend sl sr
match' (TVar u) (TVar t) | u == t = return mempty
match' (TVar mv) t | getType mv == getType t = return [(mv,t)]
--match' (TMetaVar mv) t | kind mv == kind t = return [(mv,t)]
match' (TCon tc1) (TCon tc2) | tc1==tc2 = return mempty
match' t1 t2  = fail $ "match: " ++ show (t1,t2)


-- | Appears to be based on THIH's @reduce@
splitReduce :: Set.Set MetaVar -- ^ \"fixed\" meta vars -- free in the type environment
            -> Set.Set MetaVar -- ^ \"generic\" meta vars -- we want to quantify over these
            -> [Pred]          -- ^ Relevant predicates
            -> Tc ([MetaVar], [Pred], [Pred])
               -- ^ (retained \"generic\" meta-vars, "deferred" predicates, "retained" predicates)
splitReduce fs gs ps = do
    h <- getClassHierarchy
    when (not $ Set.null fs) $ liftIO $ putStrLn "splitReduce called with non-empty fixed set"
    wdump FD.BoxySteps $ liftIO $ putStrLn $ "splitReduce " <+> pprint (fs,gs,ps)

    (ds, rs) <- splitPreds h fs ps
    wdump FD.BoxySteps $ liftIO $ putStrLn $ render $ pprint (ds,rs)

    (rs',sub) <- genDefaults h (fs `Set.union` gs) rs
    wdump FD.BoxySteps $ liftIO $ putStrLn $ render $ pprint (rs')

    flip mapM_ sub $ \ (x,y) ->  do
        let msg = "defaulting: " <+> pprint x <+> "=>" <+> prettyPrintType y
        wdump FD.BoxySteps $ liftIO $ putStrLn msg
        addWarn "type-defaults" msg
    sequence_ [ varBind x y | (x,y) <- nub sub]

    ret <- return (Set.toList gs List.\\ map fst sub, ds, rs')
    wdump FD.BoxySteps $ liftIO $ putStrLn $ render $ pprint ret
    return ret


-- | Resolve all ambiguous variables (or die trying)
withDefaults :: Monad m
             => ClassHierarchy
             -> Set.Set MetaVar -- ^ Variables to be considered known (fixed + generic)
             -> [Pred]          -- ^ Predicates to consider
             -> m [(MetaVar, [Pred], Type)]
             -- ^ List of (defaulted meta var, predicates involving it, type defaulted to)
withDefaults h vs ps
  | any null tss = fail $ "withDefaults.ambiguity: " ++ (pprint ps)  ++ pprint (Set.toList vs) -- ++ show ps
--  | otherwise = fail $ "Zambiguity: " ++ (render $ pprint ps) ++  show (ps,ps',ams)
  | otherwise    = return $ [ (v,qs,head ts) | (v,qs,ts) <- ams ]
    where ams = ambig h vs ps
          tss = [ ts | (v,qs,ts) <- ams ]

-- | Return retained predicates and a defaulting substitution
genDefaults :: Monad m
            => ClassHierarchy
            -> Set.Set MetaVar -- ^ Variables to be considered known (fixed + generic)
            -> [Pred]          -- ^ Predicates to examine
            -> m ([Pred], [(MetaVar,Type)])
genDefaults h vs ps = do
    ams <- withDefaults h vs ps
    let ps' = [ p | (v,qs,ts) <- ams, p<-qs ]
        vs  = [ (v,t)  | (v,qs,t) <- ams ]
    return (ps \\ ps',  vs)

-- @ambiguities@ from THIH + call to @candidates@
ambig :: ClassHierarchy
      -> Set.Set MetaVar -- ^ Variables to be considered known (fixed + generic)
      -> [Pred]          -- ^ Predicates to consider
      -> [(MetaVar, [Pred], [Type])] -- ^ List of (ambiguous meta var, predicates involving it, potential defaults)

ambig h vs ps
  = [ (v, qs, defs h v qs) |
         v <- Set.toList (freeMetaVarsPreds ps `Set.difference` vs),
         let qs = [ p | p<-ps, v `Set.member` freeMetaVarsPred p ] ]


assertEntailment :: Preds -> Preds -> Tc ()
assertEntailment qs ps = do
--    liftIO $ putStrLn $ "Asserting entailment: " ++ pprint (qs,ps)
    ch <- getClassHierarchy
    let ns = [ p  | p <- ps, not $ entails ch qs p ]
    if null ns then return () else
        fail $ "Signature too Weak: " ++ pprint qs ++ " does not imply " ++ pprint ns

assertEquivalant :: Preds -> Preds -> Tc ()
assertEquivalant qs ps = do
    assertEntailment qs ps
    assertEntailment ps qs
{-

reduce :: OptionMonad m => ClassHierarchy -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])

reduce h fs gs ps = do
    (ds, rs) <- split h fs ps
    rs' <-   useDefaults h (fs++gs) rs
    return (ds,rs')
-}

-- | 'candidates' from THIH
defs     :: ClassHierarchy -> MetaVar -> [Pred] -> [Type]
defs h v qs = trace ("Tc.Class.defs" <+> pprint v <+> pprint qs) $
              [ t | all ((TMetaVar v)==) ts,
                  all (`elem` stdClasses) cs, -- XXX needs fixing
                  any (`elem` numClasses) cs, -- XXX needs fixing
                  t <- defaults, -- XXX needs fixing
                  and [ entails h [] (IsIn c t) | c <- cs ]]
 where cs = [ c | (IsIn c t) <- qs ]
       ts = [ t | (IsIn c t) <- qs ]


-- | FIXME: use @default@ declarations!
defaults    :: [Type]
defaults
    | not $ fopts FO.Defaulting = []
    | otherwise = map (\name -> TCon (Tycon name kindStar)) [tc_Integer, tc_Double]

topDefaults     :: [Pred] -> Tc ()
topDefaults []  = wdump FD.BoxySteps $ liftIO $ putStrLn $ "topDefaults [] -- skipping splitReduce"
topDefaults ps  = do
    -- Don't do anything unless the monomorphism restriction is in effect
    mono <- flagOpt FO.MonomorphismRestriction
    when mono $ do

    wdump FD.BoxySteps $ liftIO $ putStrLn $ "topDefaults" <+> pprint ps
    withContext (simpleMsg "while enforcing the monomorphism restriction") $ do
        ([], [], []) <- splitReduce mempty mempty ps
        return ()
    
--      | otherwise    -> return $ Map.fromList (zip vs (map head tss))
--        where ams = ambig h [] ps
--              tss = [ ts | (v,qs,ts) <- ams ]
--              vs  = [ v  | (v,qs,ts) <- ams ]

numClasses,stdClasses :: [Name]

stdClasses = [
    class_Eq,
    class_Ord,
    class_Enum,
    class_Bounded,
    class_Show,
    class_Read,
    class_Ix,
    class_Functor,
    class_Monad,
    class_Num ,
    class_Real,
    class_Integral,
    class_Fractional,
    class_Floating,
    class_RealFrac,
    class_RealFloat
    ]

numClasses = [
    class_Num ,
    class_Real,
    class_Integral,
    class_Fractional,
    class_Floating,
    class_RealFrac,
    class_RealFloat
    ]
