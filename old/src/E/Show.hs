module E.Show(ePretty,render,prettyE,ePrettyEx) where

import Char
import Control.Monad.Identity
import Maybe

import Doc.Attr
import Doc.DocLike
import Doc.PPrint
import Doc.Pretty
import E.E
import E.FreeVars()
import E.TypeCheck
import Name.Id
import Name.Name
import Name.Names
import Name.VConsts
import Options
import Support.FreeVars
import Support.Unparse
import Util.SetLike
import Util.VarName
import qualified Doc.Chars as UC
import qualified FlagDump as FD

{-# NOINLINE render #-}
{-# NOINLINE ePretty #-}
{-# NOINLINE prettyE #-}
{-# NOINLINE ePrettyEx #-}
render :: Doc -> String
render doc =  displayS (renderPretty 100.0 (optColumns options)  doc) ""

prettyE :: E -> String
prettyE e = render $ ePretty e

ePrettyEx :: E -> Doc
ePrettyEx = ePretty

showId :: DocLike d => Id -> d
showId i | isEmptyId i = (char '_')
showId i | Just x <- fromId i  = (text $ show x)
showId i = (text $ 'x':show i)

instance DocLike d => PPrint d TVr where
    pprint TVr { tvrIdent = i }  = showId i

instance PPrint Doc E where
    pprint x = ePretty x

instance PPrint String E where
    pprint x = prettyE x

instance PPrint String e => PPrint String (Maybe e) where
    pprint Nothing = "Nothing"
    pprint (Just e) = pprint e

instance PPrint String (Lit E E) where
    pprintPrec n x | n <= 9    = prettyE (ELit x)
                   | otherwise = parens (prettyE (ELit x))

newtype SEM a = SEM { _unSEM :: VarNameT E Id String Identity a }
    deriving(Monad,Functor)

enumList :: [(Name, [String])]
enumList = [
    (tc_Boolzh,["False#","True#"]),
    (toName TypeConstructor ("Lhc.Order","Ordering#"),["LT#","EQ#","GT#"])
    ]

showLit ::
    (a -> SEM (Unparse Doc))   -- ^ routine for showing the contents of constructor literals
    -> Lit a E                 -- ^ the literal to show
    -> SEM (Unparse Doc)       -- ^ the final result
showLit showBind l = do
    let const_color = col "blue"
    let --f (LitInt c t) | t == tCharzh = return $ atom $ (const_color (tshow $ chr i)) where
        --    i = fromIntegral c
        f (LitInt i (ELit LitCons { litName = n })) | Just l <- lookup n enumList, i >= 0 && fromIntegral i < length l =
            return $ atom $ (const_color (text $ l !! (fromIntegral i)))
        f (LitInt i _) = return $ atom $ (const_color (text $ show i))
        f LitCons { litName = s, litArgs = es } | Just n <- fromTupname s , n == length es = do
            es' <- mapM (fmap unparse . showBind) es
            return $ atom $ tupled es'
        f LitCons { litName = s, litArgs = es } | Just n <- fromUnboxedNameTuple s, n == length es = do
            es' <- mapM (fmap unparse . showBind) es
            return $ atom $ encloseSep (text "(# ") (text " #)") (text ", ") es'
        -- Fully-applied tc_Arrows are not good, so make them red so that they are easy to spot
        f LitCons { litName = n, litArgs = [a,b] } | tc_Arrow == n  = do
            a' <- showBind a
            b' <- showBind b
            return $ foldl appCon (atom $ col "red" $ tshow n) [a',b']
        f LitCons { litName = n, litArgs = [a,b] } | dc_Cons == n  = do
            a' <- showBind a
            b' <- showBind b
            return $ a' `cons` b'
        f LitCons { litName = n, litArgs = [e] } | tc_List == n = do
            e <- showBind e
            return $  atom   (char '[' <> unparse e  <> char ']')
        f LitCons { litName = n, litArgs = [] } | dc_EmptyList == n = return $ atom $ text "[]"
        f LitCons { litName = n, litArgs = [v] }
            | n == dc_Integer = go "Integer#"
            | n == dc_Int     = go "Int#"
            | n == dc_Char    = go "Char#"
          where go n = do
                    se <- showBind v
                    return $ atom (text n) `app` se
        f LitCons { litName = s, litArgs = es, litType = t, litAliasFor = Just af } | dump FD.EAlias = do
            es' <- mapM showBind es
            se <- showE af
            return $ foldl appCon (atom (tshow s <> char '@' <> parens (unparse se))) es' -- `inhabit` prettye t
        f LitCons { litName = s, litArgs = es, litType = t } = do
            es' <- mapM showBind es
            return $ foldl appCon (atom (tshow s)) es' -- `inhabit` prettye t
        cons = bop (R,5) (text ":")
    f l

app, appCon :: Unparse Doc -> Unparse Doc -> Unparse Doc
app = bop (L,100) (text " ")
appCon = bop (L,99) (text " ")
col :: String -> Doc -> Doc
col n x = attrColor attr n x
attr :: Attr
attr = if dump FD.Html then html else ansi

showI :: DocLike d => Id -> SEM d
showI i = do
    n <- SEM $ maybeLookupName i
    case n of
        Nothing -> showId i
        Just n -> text n


showTVr :: TVr -> SEM (Unparse Doc)
showTVr TVr { tvrIdent = i, tvrType =  t, tvrInfo = nfo}  = do
    let si = if dump FD.EInfo then (<> tshow nfo) else id
    ty <- showE t
    ii <- showI i
    return $ atom (si ii) `inhabit` ty
showTVr' :: TVr' t -> SEM (Unparse Doc)
showTVr' TVr { tvrIdent = i} = do
    ii <- showI i
    return $ atom ii


allocTVr :: TVr -> SEM a -> SEM a
allocTVr _tvr action | dump FD.EVerbose = action
allocTVr tvr action | isEmptyId (tvrIdent tvr) = action
allocTVr tvr (SEM action) | tvrType tvr == eStar  = do
    SEM $ subVarName $ newName (map (:[]) ['a' ..]) eStar (tvrIdent tvr) >> action
allocTVr tvr (SEM action) | tvrType tvr == eStar `tFunc` eStar  = do
    SEM $ subVarName $ newName (map (('f':) . show) [0::Int ..])  (tvrType tvr) (tvrIdent tvr) >> action
allocTVr tvr (SEM action) | not $ idIsNamed (tvrIdent tvr) = do
    SEM $ subVarName $ newName (map (('v':) . show) [1::Int ..]) Unknown (tvrIdent tvr) >> action
allocTVr _ action = action

-- FIXME this should not be here!
tBoolzh :: E
tBoolzh = ELit litCons { litName = tc_Boolzh, litType = eHash, litAliasFor = Just tIntzh }

-- collects lambda and pi abstractions
collectAbstractions :: TextLike a => E -> ([(a, TVr, Bool)], E)
collectAbstractions e0 = go e0 [] where
    go e1@(EPi tvr e)  xs | isEmptyId (tvrIdent tvr)         = done e1 xs
                          | not (sortKindLike (tvrType tvr)) = go e ((UC.pI,     tvr, True) :xs)
                          | tvrType tvr /= eStar             = go e ((UC.forall, tvr, True) :xs)
                          | dump FD.EVerbose || tvrIdent tvr `member` (freeVars e::IdSet)
                                                             = go e ((UC.forall, tvr, False):xs)
                          | otherwise                        = done e1 xs
    go e1@(ELam tvr e) xs | tvrType tvr == eStar             = go e ((UC.lAmbda, tvr, False):xs)
                          | sortKindLike (tvrType tvr)       = go e ((UC.lAmbda, tvr, True) :xs)
                          | otherwise                        = go e ((UC.lambda, tvr, True) :xs)
    go  e           xs = done e xs
    done e xs = (reverse xs, e)
                                                  
showE :: E -> SEM (Unparse Doc)
showE e = do
    let const_color = col "blue"
    let f e | Just s <- E.E.toString e = return $ atom $ const_color (text $ show s)
        f e | Just xs <- eToList e = do
            xs <- mapM (fmap unparse . showE) xs
            return $ atom $ list xs
        f e | e == tBool     = return $ atom $ text "Bool"
        f e | e == tBoolzh   = return $ atom $ text "Bool#"
        f e | e == tChar     = return $ atom $ text "Char"
        f e | e == tInt      = return $ atom $ text "Int"
        f e | e == tInteger  = return $ atom $ text "Integer"
        f e | e == tRational = return $ atom $ text "Rational"
        f e | e == tString   = return $ atom $ text "String"
        f e | e == tUnit     = return $ atom $ text "()"
        --f e | e == tWorld__  = return $ atom $ text "World__"
        f e | e == vFalse    = return $ atom $ text "False"
        f e | e == vTrue     = return $ atom $ text "True"
        f e | e == vUnit     = return $ atom $ text "()"
        f (EAp a b) = liftM2 app (showE a) (showE b)
        f (EPi (TVr { tvrIdent = n, tvrType =  e1}) e2) | isEmptyId n = liftM2 arr (showE e1) (showE e2)
        f (EPi (TVr { tvrIdent = n, tvrType =  e1}) e2) | not $ dump FD.EVerbose, not $ n `member` (freeVars e2 ::IdSet) = liftM2 arr (showE e1) (showE e2)
        f e0 | (as@(_:_), e) <- collectAbstractions e0 =
            foldr (\(_, tvr, _) -> allocTVr tvr)
                  (do tops <- mapM p as
                      e <- showE e
                      return (atom $ group $ (align $ skipToNest <> fillCat tops) <$> unparse e))
                  as
            where 
              p :: (Doc, TVr, Bool) -> SEM Doc
              p (c,t,detailed) = do tvr <- if detailed then showTVr t else showTVr' t
                                    return (retOp c <> unparse tvr <> retOp (char '.'))
        f (EVar tvr) = if dump FD.EVerbose then showTVr tvr else showTVr' tvr
        f Unknown = return $ symbol (char  '?')
        f (ESort s) = return $ symbol (tshow s)
        f (ELit l) = showLit showE l
        f (EError "" t) = do
            ty <- showE t
            return $ atom $ angles (text "exitFailure"  <>  UC.coloncolon <> unparse ty)
        f (EError s t) = do
            ty <- showE t
            return $ atom $ angles ( UC.bottom <> char ':' <> text s <>  UC.coloncolon <> unparse ty)
        f (EPrim s es t) = do
            es' <- mapM showE es
            t <- showE t
            return $ atom $ angles $ unparse $ foldl app (atom (pprint s)) es' `inhabit` t
        f ELetRec { eDefs = ds, eBody = e } = foldr (\(tvr,_) -> allocTVr tvr) (do
            e <- fmap unparse $ showE e
            ds <- mapM (fmap unparse . showDecl) ds
            return $ fixitize (L,(-10)) $ atom $ nest 4 (group ( keyword "let"
                                                                  <$> (align $ sep (map (<> bc ';') ds))
                                                                  <$> (keyword "in")) </> e )) ds

        f ec@(ECase { eCaseScrutinee = e, eCaseAlts = alts }) = mt (showE (eCaseType ec)) $  allocTVr (eCaseBind ec) $ do
            scrut <- fmap unparse $ showE e
            alts <- mapM showAlt alts
            let ecb = eCaseBind ec
                isUsed = tvrIdent ecb `member` (freeVars (caseBodies ec) :: IdSet)
            db <- showTVr (if dump FD.EVerbose || isUsed then ecb else ecb { tvrIdent = emptyId })
            dcase <- case (eCaseDefault ec) of
                Nothing -> return []
                Just e -> do
                    e <- showE e
                    return [unparse db <+> UC.rArrow <+> unparse e]
            let alts' = map (<> bc ';') (alts ++ dcase)
            let mbind | isJust (eCaseDefault ec) = empty
                      | (isUsed && isNothing (eCaseDefault ec)) || dump FD.EVerbose = text " " <> (if isUsed then id else (char '_' <>)) (unparse db) <+> text "<-"
                      | otherwise = empty
            return $ fixitize ((L,(-10))) $ atom $
                group (nest 4 ( keyword "case" <> mbind <+> scrut <+> keyword "of" <$>  (align $ vcat (alts'))) )
        showAlt (Alt l e) = foldr allocTVr ans (litBinds l) where
            ans = do
                l <- showLit showTVr l
                e <- showE e
                return $ fill 10 ((unparse l) <+>  UC.rArrow </> (unparse e))
        showDecl (t,e) = do
            t <- subSEM $ showTVr t
            e <- subSEM $ showE e
            return $ atom $ unparse t <+> retOp (char '=') </> unparse e
        bold' = bold
        bc = bold' . char
        keyword x = col "magenta" (text x)
        symbol x = atom (bold' x)
        arr = bop (R,0) $ retOp (space <> UC.rArrow <> space)
        dot = bop (R,-1) $ retOp (char '.')

        mt t x | dump FD.EVerbose = do
                    t <- t
                    x <- x
                    return $ x `inhabit` t
        mt _ x = x


    f e

subSEM :: SEM a -> SEM a
subSEM (SEM act) = SEM $ subVarName act


retOp :: Doc -> Doc
retOp x = col "lightgreen" x
inhabit :: Unparse Doc -> Unparse Doc -> Unparse Doc
inhabit = bop (N,-2) $ retOp UC.coloncolon
bold :: Doc -> Doc
bold = attrBold attr

ePretty :: E -> Doc
ePretty e = unparse pe where
    (SEM pe') = showE e
    Identity pe = runVarNameT pe'

-- skip to the current nesting level, breaking the line if already past it
skipToNest :: Doc
skipToNest      = column (\k ->
                  nesting (\i -> if k > i
                                 then linebreak
                                 else text (replicate (i-k) ' ')))