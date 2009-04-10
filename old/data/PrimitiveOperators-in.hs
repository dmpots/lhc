
{- This file is generated -}
module PrimitiveOperators(
    primitiveInsts,
    constantMethods,
    create_uintegralCast_toInt,
    create_uintegralCast_fromInt,
    allCTypes
    ) where

import Data.Monoid

import C.Arch
import C.Prims
import E.E
import E.TypeCheck()
import E.Values
import FrontEnd.Tc.Type
import Name.Name
import Name.Prim
import Name.VConsts
import Support.CanType
import qualified Cmm.Op as Op
import qualified Cmm.Number as Num
import Name.Id

nameToOpTy :: Monad m => Name -> m Op.Ty
nameToOpTy n = do RawType <- return $ nameType n; Op.readTy (show n)

tPtr :: E -> E
tPtr t = ELit (litCons { litName = tc_Ptr
                       , litArgs = [t]
                       , litType = eStar
                       , litAliasFor = Just (ELam tvr { tvrIdent = anonymous 2
                                                      , tvrType = eStar}
                                             (ELit litCons { litName = tc_Addr, litType = eStar })) })

create_integralCast :: Op.ConvOp -> Name -> E -> Name -> E -> E -> E -> E
create_integralCast conv c1 t1 c2 t2 e t = eCase e [Alt (litCons { litName = c1, litArgs = [tvra], litType = te }) cc] Unknown  where
    te = getType e
    ELit LitCons { litName = n1, litArgs = [] } = t1
    ELit LitCons { litName = n2, litArgs = [] } = t2
    Just n1' = nameToOpTy n1
    Just n2' = nameToOpTy n2
    tvra =  tVr (anonymous 4) t1
    tvrb =  tVr (anonymous 6) t2
    cc = if n1 == n2 then ELit (litCons { litName = c2, litArgs = [EVar tvra], litType = t }) else
        eStrictLet  tvrb (EPrim (APrim (Op (Op.ConvOp conv n1') n2') mempty) [EVar tvra] t2)  (ELit (litCons { litName = c2, litArgs = [EVar tvrb], litType = t }))

create_integralCast_toInt :: Name -> E -> E -> E
create_integralCast_toInt c1 t1 e = create_integralCast Op.I2I c1 t1 dc_Int tIntzh e tInt
create_integralCast_toInteger :: Name -> E -> E -> E
create_integralCast_toInteger c1 t1 e = create_integralCast Op.Sx c1 t1 dc_Integer tIntegerzh e tInteger
create_integralCast_fromInt :: Name -> E -> E -> E -> E
create_integralCast_fromInt c2 t2 e t = create_integralCast Op.I2I dc_Int tIntzh c2 t2 e t
create_integralCast_fromInteger :: Name -> E -> E -> E -> E
create_integralCast_fromInteger c2 t2 e t = create_integralCast Op.Lobits dc_Integer tIntegerzh c2 t2 e t

create_uintegralCast_toInt :: Name -> E -> E -> E
create_uintegralCast_toInt c1 t1 e = create_integralCast Op.U2U c1 t1 dc_Int tIntzh e tInt
create_uintegralCast_toInteger :: Name -> E -> E -> E
create_uintegralCast_toInteger c1 t1 e = create_integralCast Op.Zx c1 t1 dc_Integer tIntegerzh e tInteger
create_uintegralCast_fromInt :: Name -> E -> E -> E -> E
create_uintegralCast_fromInt c2 t2 e t = create_integralCast Op.U2U dc_Int tIntzh c2 t2 e t
create_uintegralCast_fromInteger :: Name -> E -> E -> E -> E
create_uintegralCast_fromInteger c2 t2 e t = create_integralCast Op.Lobits dc_Integer tIntegerzh c2 t2 e t

create_fintegralCast_fromInt :: Name -> E -> E -> E -> E
create_fintegralCast_fromInt c2 t2 e t = create_integralCast Op.I2F dc_Int tIntzh c2 t2 e t
create_fintegralCast_fromInteger :: Name -> E -> E -> E -> E
create_fintegralCast_fromInteger c2 t2 e t = create_integralCast Op.I2F dc_Integer tIntegerzh c2 t2 e t


toClassName :: String -> Name
toClassName x = parseName ClassName x

toInstName :: String -> Name
toInstName x = toName Val ("Instance@",'i':x)

unbox' :: E -> Name -> TVr -> E -> E
unbox' e cn tvr wtd = eCase e [Alt (litCons { litName = cn, litArgs = [tvr], litType = te }) wtd] Unknown where
    te = getType e

binOp :: Op.BinOp -> Op.Ty -> Op.Ty -> Op.Ty -> APrim
binOp op ca cb cr = APrim (Op (Op.BinOp op ca cb) cr) mempty

oper_aa :: Op.UnOp -> ExtType -> E -> E
oper_aa op ct' e = EPrim (APrim (Op (Op.UnOp op ct) ct) mempty) [e] (rawType ct') where
    ct = stringToOpTy ct'
oper_aaB :: Op.BinOp -> ExtType -> E -> E -> E
oper_aaB op ct' a b = EPrim (binOp op ct ct ot_int) [a,b] tBoolzh where
    ct = stringToOpTy ct'
oper_aaa :: Op.BinOp -> ExtType -> E -> E -> E
oper_aaa op ct' a b = EPrim (binOp op ct ct ct) [a,b] (rawType ct') where
    ct = stringToOpTy ct'
oper_aIa :: Op.BinOp -> ExtType -> E -> E -> E
oper_aIa op ct' a b = EPrim (binOp op ct ot_int ct) [a,b] (rawType ct') where
    ct = stringToOpTy ct'

--zeroI =  LitInt 0 intt

ot_int :: Op.Ty
ot_int = stringToOpTy "bits32"

op_aIa :: Op.BinOp -> ExtType -> Name -> E -> E
op_aIa op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') dc_Int tvrb wtd))) where
    tvra' = tVr (anonymous 2) t
    tvrb' = tVr (anonymous 4) tInt
    tvra = tVr (anonymous 6) st
    tvrb = tVr (anonymous 8) intt
    tvrc = tVr (anonymous 10) st
    st = rawType ct
    intt = rawType "bits32"
    wtd = eStrictLet tvrc (oper_aIa op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = t })
op_aaa :: Op.BinOp -> ExtType -> Name -> E -> E
op_aaa op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr (anonymous 2) t
    tvrb' = tVr (anonymous 4) t
    tvra = tVr (anonymous 6) st
    tvrb = tVr (anonymous 8) st
    tvrc = tVr (anonymous 10) st
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaa op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = t })
op_aa :: Op.UnOp -> ExtType -> Name -> E -> E
op_aa op ct cn t = ELam tvra' (unbox' (EVar tvra') cn tvra wtd) where
    tvra' = tVr (anonymous 2) t
    tvra = tVr (anonymous 6) st
    tvrc = tVr (anonymous 10) st
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aa op ct (EVar tvra)) (rebox (EVar tvrc))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = t })
--op_aaI op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
--    tvra' = tVr 2 t
--    tvrb' = tVr 4 t
--    tvra = tVr 6 st
--    tvrb = tVr 8 st
--    tvrc = tVr 10 intt
--    st = rawType ct
--    wtd = eStrictLet tvrc (oper_aaI op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
--    rebox x = ELit (litCons { litName = dc_Int, litArgs = [x], litType = t })

op_aaB :: Op.BinOp -> ExtType -> Name -> E -> E
op_aaB op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr (anonymous 2) t
    tvrb' = tVr (anonymous 4) t
    tvra = tVr (anonymous 6) st
    tvrb = tVr (anonymous 8) st
    tvrc = tVr (anonymous 10) tBoolzh
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaB op ct (EVar tvra) (EVar tvrb)) (ELit (litCons { litName = dc_Boolzh, litArgs = [EVar tvrc], litType = tBool }))  -- (caseof (EVar tvrc))
--    caseof x = eCase x [Alt zeroI vFalse]  vTrue

build_abs :: ExtType -> Name -> E -> E
build_abs ct cn v = unbox' v cn tvra (eCase (oper_aaB Op.Lt ct (EVar tvra) zero)  [Alt lFalsezh (rebox $ EVar tvra), Alt lTruezh fs] Unknown) where
    te = getType v
    tvra = tVr (anonymous 2) st
    tvrb = tVr (anonymous 4) st
    zero = ELit $ LitInt 0 st
    st = rawType ct
    fs = eStrictLet tvrb (oper_aa Op.Neg ct (EVar tvra)) (rebox (EVar tvrb))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_uabs :: ExtType -> Name -> E -> E
build_uabs ct cn v = v

build_fabs :: ExtType -> Name -> E -> E
build_fabs ct cn v = unbox' v cn tvra (rebox (oper_aa Op.FAbs ct (EVar tvra))) where
    te = getType v
    tvra = tVr (anonymous 2) st
    st = rawType ct
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_usignum :: ExtType -> Name -> E -> E
build_usignum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (rebox (ELit one))) where
    tvra = tVr (anonymous 2) st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = LitInt 1 st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_signum :: ExtType -> Name -> E -> E
build_signum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (eCase (oper_aaB Op.Lt ct (EVar tvra) (ELit zero)) [Alt lFalsezh (rebox one),Alt lTruezh (rebox negativeOne)] Unknown)) where
    tvra = tVr (anonymous 2) st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = ELit $ LitInt 1 st
    negativeOne = ELit $ LitInt (-1) st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })


build_fsignum :: ExtType -> Name -> E -> E
build_fsignum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (eCase (oper_aaB Op.FLt ct (EVar tvra) (ELit zero)) [Alt lFalsezh (rebox one),Alt lTruezh (rebox negativeOne)] Unknown)) where
    tvra = tVr (anonymous 2) st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = ELit $ LitInt 1 st
    negativeOne = ELit $ LitInt (-1) st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })


buildPeek :: Name -> E -> ExtType -> E
buildPeek cn t p = ELam tvr $ ELam tvrWorld (unbox' (EVar tvr) dc_Addr tvr' rest)  where
    tvr = (tVr (anonymous 2) (tPtr t))
    tvr' = tVr (anonymous 4) (rawType "bits<ptr>")
    tvrWorld2 = tVr (anonymous 258) tWorld__
    tvrWorld = tVr (anonymous 256) tWorld__
    rtVar = tVr (anonymous 260) (rawType p)
    rtVar' = tVr (anonymous 262) t
    rest = eCaseTup' (EPrim (APrim (Peek (stringToOpTy p)) mempty) [EVar tvrWorld, EVar tvr'] (ltTuple' [tWorld__,rawType p])) [tvrWorld2,rtVar] (eLet rtVar' (ELit $ litCons { litName = cn, litArgs = [EVar rtVar], litType = t }) $ eJustIO (EVar tvrWorld2) (EVar rtVar') )


buildPoke :: Name -> E -> ExtType -> E
buildPoke cn t p = ELam ptr_tvr $ ELam v_tvr $ createIO_ $ (\tw -> unbox' (EVar ptr_tvr) dc_Addr ptr_tvr' $ unbox' (EVar v_tvr) cn v_tvr' $ EPrim (APrim (Poke (stringToOpTy p)) mempty) [EVar tw, EVar ptr_tvr', EVar v_tvr'] tWorld__) where
    ptr_tvr =  (tVr (anonymous 2) (tPtr t))
    v_tvr = tVr (anonymous 4) t
    ptr_tvr' =  (tVr (anonymous 6) (rawType "bits<ptr>"))
    v_tvr' = tVr (anonymous 8) (rawType p)

toIO :: E -> E -> E
toIO t x = x

{-
createIO t pv = toIO t (ELam tvrWorld $  eCaseTup  (pv tvrWorld) [tvrWorld2,rtVar] (eJustIO (EVar tvrWorld2) (EVar rtVar))) where
    tvrWorld2 = tVr 258 tWorld__
    tvrWorld = tVr 256 tWorld__
    rtVar = tVr 260 t
-}
createIO_ :: (TVr -> E) -> E
createIO_ pv = toIO tUnit (ELam tvrWorld $  eStrictLet tvrWorld2 (pv tvrWorld)  (eJustIO (EVar tvrWorld2) vUnit)) where
    tvrWorld2 = tVr (anonymous 258) tWorld__
    tvrWorld = tVr (anonymous 256) tWorld__


prim_number :: Name -> Num.Number -> E -> E -> E
prim_number cn v t et = ELit litCons { litName = cn, litArgs = [ELit (LitInt v t)], litType = et }

prim_minbound, prim_maxbound, prim_uminbound, prim_umaxbound :: Name -> E -> ExtType ->  E
prim_uminbound dc dt s = prim_number dc 0 (rawType s) dt
prim_umaxbound = prim_bound PrimUMaxBound
prim_maxbound = prim_bound PrimMaxBound
prim_minbound = prim_bound PrimMinBound

prim_bound :: PrimTypeInfo -> Name -> E -> String -> E
prim_bound pt dc dt s = (ELit (litCons { litName = dc, litArgs = [rp], litType = dt })) where
    rt = rawType s
    Just at = Op.readTy s
    rp | Just n <- primStaticTypeInfo at pt = (ELit (LitInt (fromInteger n) rt))
       | otherwise = EPrim (APrim (PrimTypeInfo { primArgTy = at, primRetTy = at, primTypeInfo = pt }) mempty) [] rt

prim_sizeof :: String -> E
prim_sizeof s = (ELit (litCons { litName = dc_Int, litArgs = [rp], litType = tInt })) where
    Just at = Op.readTy s
    rp | Just n <- primStaticTypeInfo at PrimSizeOf = (ELit (LitInt (fromInteger n) tIntzh))
       | otherwise = EPrim (APrim (PrimTypeInfo { primArgTy = stringToOpTy s, primRetTy = ot_int, primTypeInfo = PrimSizeOf }) mempty) [] tIntzh


v2_Int, v2_Integer :: TVr
v2_Int = tVr (anonymous 2) tInt
v2_Integer = tVr (anonymous 2) tInteger
v2 :: e -> TVr' e
v2 t = tVr (anonymous 2) t

v0 :: e -> TVr' e
v0 t = tVr emptyId t

{-# NOINLINE constantMethods #-}
{-# NOINLINE primitiveInsts #-}
{-# NOINLINE allCTypes #-}
