
{- This file is generated -}
module PrimitiveOperators(
    primitiveInsts,
    constantMethods,
    create_uintegralCast_toInt,
    create_uintegralCast_fromInt,
    theMethods,
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
import Name.Id

nameToOpTy n = do RawType <- return $ nameType n; Op.readTy (show n)

tPtr t = ELit (litCons { litName = tc_Ptr
                       , litArgs = [t]
                       , litType = eStar
                       , litAliasFor = Just (ELam tvr { tvrIdent = anonymous 2
                                                      , tvrType = eStar}
                                             (ELit litCons { litName = tc_Addr, litType = eStar })) })

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

create_integralCast_toInt c1 t1 e = create_integralCast Op.I2I c1 t1 dc_Int tIntzh e tInt
create_integralCast_toInteger c1 t1 e = create_integralCast Op.Sx c1 t1 dc_Integer tIntegerzh e tInteger
create_integralCast_fromInt c2 t2 e t = create_integralCast Op.I2I dc_Int tIntzh c2 t2 e t
create_integralCast_fromInteger c2 t2 e t = create_integralCast Op.Lobits dc_Integer tIntegerzh c2 t2 e t

create_uintegralCast_toInt c1 t1 e = create_integralCast Op.U2U c1 t1 dc_Int tIntzh e tInt
create_uintegralCast_toInteger c1 t1 e = create_integralCast Op.Zx c1 t1 dc_Integer tIntegerzh e tInteger
create_uintegralCast_fromInt c2 t2 e t = create_integralCast Op.U2U dc_Int tIntzh c2 t2 e t
create_uintegralCast_fromInteger c2 t2 e t = create_integralCast Op.Lobits dc_Integer tIntegerzh c2 t2 e t

create_fintegralCast_fromInt c2 t2 e t = create_integralCast Op.I2F dc_Int tIntzh c2 t2 e t
create_fintegralCast_fromInteger c2 t2 e t = create_integralCast Op.I2F dc_Integer tIntegerzh c2 t2 e t


toClassName x = parseName ClassName x

toInstName x = toName Val ("Instance@",'i':x)

unbox' e cn tvr wtd = eCase e [Alt (litCons { litName = cn, litArgs = [tvr], litType = te }) wtd] Unknown where
    te = getType e

binOp op ca cb cr = APrim (Op (Op.BinOp op ca cb) cr) mempty

oper_aa op ct' e = EPrim (APrim (Op (Op.UnOp op ct) ct) mempty) [e] (rawType ct') where
    ct = stringToOpTy ct'
oper_aaB op ct' a b = EPrim (binOp op ct ct ot_int) [a,b] tBoolzh where
    ct = stringToOpTy ct'
oper_aaa op ct' a b = EPrim (binOp op ct ct ct) [a,b] (rawType ct') where
    ct = stringToOpTy ct'
oper_aIa op ct' a b = EPrim (binOp op ct ot_int ct) [a,b] (rawType ct') where
    ct = stringToOpTy ct'

--zeroI =  LitInt 0 intt

ot_int = stringToOpTy "bits32"

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
op_aaa op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr (anonymous 2) t
    tvrb' = tVr (anonymous 4) t
    tvra = tVr (anonymous 6) st
    tvrb = tVr (anonymous 8) st
    tvrc = tVr (anonymous 10) st
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaa op ct (EVar tvra) (EVar tvrb)) (rebox (EVar tvrc))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = t })
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

op_aaB op ct cn t = ELam tvra' (ELam tvrb' (unbox' (EVar tvra') cn tvra (unbox' (EVar tvrb') cn tvrb wtd))) where
    tvra' = tVr (anonymous 2) t
    tvrb' = tVr (anonymous 4) t
    tvra = tVr (anonymous 6) st
    tvrb = tVr (anonymous 8) st
    tvrc = tVr (anonymous 10) tBoolzh
    st = rawType ct
    wtd = eStrictLet tvrc (oper_aaB op ct (EVar tvra) (EVar tvrb)) (ELit (litCons { litName = dc_Boolzh, litArgs = [EVar tvrc], litType = tBool }))  -- (caseof (EVar tvrc))
--    caseof x = eCase x [Alt zeroI vFalse]  vTrue

build_abs ct cn v = unbox' v cn tvra (eCase (oper_aaB Op.Lt ct (EVar tvra) zero)  [Alt lFalsezh (rebox $ EVar tvra), Alt lTruezh fs] Unknown) where
    te = getType v
    tvra = tVr (anonymous 2) st
    tvrb = tVr (anonymous 4) st
    zero = ELit $ LitInt 0 st
    st = rawType ct
    fs = eStrictLet tvrb (oper_aa Op.Neg ct (EVar tvra)) (rebox (EVar tvrb))
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_uabs ct cn v = v

build_fabs ct cn v = unbox' v cn tvra (rebox (oper_aa Op.FAbs ct (EVar tvra))) where
    te = getType v
    tvra = tVr (anonymous 2) st
    st = rawType ct
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_usignum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (rebox (ELit one))) where
    tvra = tVr (anonymous 2) st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = LitInt 1 st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })

build_signum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (eCase (oper_aaB Op.Lt ct (EVar tvra) (ELit zero)) [Alt lFalsezh (rebox one),Alt lTruezh (rebox negativeOne)] Unknown)) where
    tvra = tVr (anonymous 2) st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = ELit $ LitInt 1 st
    negativeOne = ELit $ LitInt (-1) st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })


build_fsignum ct cn v = unbox' v cn tvra (eCase (EVar tvra) [Alt zero (rebox (ELit zero))] (eCase (oper_aaB Op.FLt ct (EVar tvra) (ELit zero)) [Alt lFalsezh (rebox one),Alt lTruezh (rebox negativeOne)] Unknown)) where
    tvra = tVr (anonymous 2) st
    te = getType v
    st = rawType ct
    zero :: Lit a E
    zero = LitInt 0 st
    one = ELit $ LitInt 1 st
    negativeOne = ELit $ LitInt (-1) st
    rebox x = ELit (litCons { litName = cn, litArgs = [x], litType = te })


buildPeek cn t p = ELam tvr $ ELam tvrWorld (unbox' (EVar tvr) dc_Addr tvr' rest)  where
    tvr = (tVr (anonymous 2) (tPtr t))
    tvr' = tVr (anonymous 4) (rawType "bits<ptr>")
    tvrWorld2 = tVr (anonymous 258) tWorld__
    tvrWorld = tVr (anonymous 256) tWorld__
    rtVar = tVr (anonymous 260) (rawType p)
    rtVar' = tVr (anonymous 262) t
    rest = eCaseTup' (EPrim (APrim (Peek (stringToOpTy p)) mempty) [EVar tvrWorld, EVar tvr'] (ltTuple' [tWorld__,rawType p])) [tvrWorld2,rtVar] (eLet rtVar' (ELit $ litCons { litName = cn, litArgs = [EVar rtVar], litType = t }) $ eJustIO (EVar tvrWorld2) (EVar rtVar') )


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
createIO_ pv = toIO tUnit (ELam tvrWorld $  eStrictLet tvrWorld2 (pv tvrWorld)  (eJustIO (EVar tvrWorld2) vUnit)) where
    tvrWorld2 = tVr (anonymous 258) tWorld__
    tvrWorld = tVr (anonymous 256) tWorld__


prim_number cn v t et = ELit litCons { litName = cn, litArgs = [ELit (LitInt v t)], litType = et }

prim_minbound, prim_maxbound, prim_uminbound, prim_umaxbound :: Name -> E -> ExtType ->  E
prim_uminbound dc dt s = prim_number dc 0 (rawType s) dt
prim_umaxbound = prim_bound PrimUMaxBound
prim_maxbound = prim_bound PrimMaxBound
prim_minbound = prim_bound PrimMinBound

prim_bound pt dc dt s = (ELit (litCons { litName = dc, litArgs = [rp], litType = dt })) where
    rt = rawType s
    Just at = Op.readTy s
    rp | Just n <- primStaticTypeInfo at pt = (ELit (LitInt (fromInteger n) rt))
       | otherwise = EPrim (APrim (PrimTypeInfo { primArgTy = at, primRetTy = at, primTypeInfo = pt }) mempty) [] rt

prim_sizeof s = (ELit (litCons { litName = dc_Int, litArgs = [rp], litType = tInt })) where
    Just at = Op.readTy s
    rp | Just n <- primStaticTypeInfo at PrimSizeOf = (ELit (LitInt (fromInteger n) tIntzh))
       | otherwise = EPrim (APrim (PrimTypeInfo { primArgTy = stringToOpTy s, primRetTy = ot_int, primTypeInfo = PrimSizeOf }) mempty) [] tIntzh


v2_Int = tVr (anonymous 2) tInt
v2_Integer = tVr (anonymous 2) tInteger
v2 t = tVr (anonymous 2) t

v0 t = tVr emptyId t

{-# NOINLINE constantMethods #-}
{-# NOINLINE primitiveInsts #-}
{-# NOINLINE allCTypes #-}


primitiveInsts = [
   [] :=> IsIn n_Lhc_Enum_Bounded tc_Lhc_Basics_Integer
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Lhc_Basics_Integer
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Lhc_Basics_Integer
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Lhc_Basics_Integer
  ,[] :=> IsIn n_Lhc_Num_Num tc_Lhc_Basics_Integer
  ,[] :=> IsIn n_Data_Bits_Bits tc_Lhc_Basics_Integer
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Lhc_Basics_Integer
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Int_Int8
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Int_Int8
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Int_Int8
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Int_Int8
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Int_Int8
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Int_Int8
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Int_Int8
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Int_Int16
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Int_Int16
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Int_Int16
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Int_Int16
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Int_Int16
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Int_Int16
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Int_Int16
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Int_Int32
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Int_Int32
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Int_Int32
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Int_Int32
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Int_Int32
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Int_Int32
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Int_Int32
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Int_Int64
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Int_Int64
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Int_Int64
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Int_Int64
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Int_Int64
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Int_Int64
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Int_Int64
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Int_IntMax
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Int_IntMax
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Int_IntMax
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Int_IntMax
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Int_IntMax
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Int_IntMax
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Int_IntMax
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Int_IntPtr
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Int_IntPtr
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Int_IntPtr
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Int_IntPtr
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Int_IntPtr
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Int_IntPtr
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Int_IntPtr
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Word_Word
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Word_Word
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Word_Word
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Word_Word
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Word_Word
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Word_Word
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Word_Word
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Word_Word8
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Word_Word8
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Word_Word8
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Word_Word8
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Word_Word8
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Word_Word8
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Word_Word8
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Word_Word16
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Word_Word16
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Word_Word16
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Word_Word16
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Word_Word16
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Word_Word16
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Word_Word16
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Word_Word32
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Word_Word32
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Word_Word32
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Word_Word32
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Word_Word32
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Word_Word32
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Word_Word32
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Word_Word64
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Word_Word64
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Word_Word64
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Word_Word64
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Word_Word64
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Word_Word64
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Word_Word64
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Word_WordMax
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Word_WordMax
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Word_WordMax
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Word_WordMax
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Word_WordMax
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Word_WordMax
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Word_WordMax
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Data_Word_WordPtr
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Data_Word_WordPtr
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Data_Word_WordPtr
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Data_Word_WordPtr
  ,[] :=> IsIn n_Lhc_Num_Num tc_Data_Word_WordPtr
  ,[] :=> IsIn n_Data_Bits_Bits tc_Data_Word_WordPtr
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Data_Word_WordPtr
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Foreign_C_Types_CChar
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Foreign_C_Types_CChar
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Foreign_C_Types_CChar
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Foreign_C_Types_CChar
  ,[] :=> IsIn n_Lhc_Num_Num tc_Foreign_C_Types_CChar
  ,[] :=> IsIn n_Data_Bits_Bits tc_Foreign_C_Types_CChar
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Foreign_C_Types_CChar
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Foreign_C_Types_CShort
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Foreign_C_Types_CShort
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Foreign_C_Types_CShort
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Foreign_C_Types_CShort
  ,[] :=> IsIn n_Lhc_Num_Num tc_Foreign_C_Types_CShort
  ,[] :=> IsIn n_Data_Bits_Bits tc_Foreign_C_Types_CShort
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Foreign_C_Types_CShort
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Foreign_C_Types_CInt
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Foreign_C_Types_CInt
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Foreign_C_Types_CInt
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Foreign_C_Types_CInt
  ,[] :=> IsIn n_Lhc_Num_Num tc_Foreign_C_Types_CInt
  ,[] :=> IsIn n_Data_Bits_Bits tc_Foreign_C_Types_CInt
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Foreign_C_Types_CInt
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Foreign_C_Types_CUInt
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Foreign_C_Types_CUInt
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Foreign_C_Types_CUInt
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Foreign_C_Types_CUInt
  ,[] :=> IsIn n_Lhc_Num_Num tc_Foreign_C_Types_CUInt
  ,[] :=> IsIn n_Data_Bits_Bits tc_Foreign_C_Types_CUInt
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Foreign_C_Types_CUInt
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Foreign_C_Types_CSize
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Foreign_C_Types_CSize
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Foreign_C_Types_CSize
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Foreign_C_Types_CSize
  ,[] :=> IsIn n_Lhc_Num_Num tc_Foreign_C_Types_CSize
  ,[] :=> IsIn n_Data_Bits_Bits tc_Foreign_C_Types_CSize
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Foreign_C_Types_CSize
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Foreign_C_Types_CWchar
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Foreign_C_Types_CWchar
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Foreign_C_Types_CWchar
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Foreign_C_Types_CWchar
  ,[] :=> IsIn n_Lhc_Num_Num tc_Foreign_C_Types_CWchar
  ,[] :=> IsIn n_Data_Bits_Bits tc_Foreign_C_Types_CWchar
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Foreign_C_Types_CWchar
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Foreign_C_Types_CWint
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Foreign_C_Types_CWint
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Foreign_C_Types_CWint
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Foreign_C_Types_CWint
  ,[] :=> IsIn n_Lhc_Num_Num tc_Foreign_C_Types_CWint
  ,[] :=> IsIn n_Data_Bits_Bits tc_Foreign_C_Types_CWint
  ,[] :=> IsIn n_Lhc_Num_Integral tc_Foreign_C_Types_CWint
  ,[] :=> IsIn n_Lhc_Enum_Bounded tc_Foreign_C_Types_CTime
  ,[] :=> IsIn n_Foreign_Storable_Storable tc_Foreign_C_Types_CTime
  ,[] :=> IsIn n_Lhc_Order_Eq tc_Foreign_C_Types_CTime
  ,[] :=> IsIn n_Lhc_Order_Ord tc_Foreign_C_Types_CTime
  ,[] :=> IsIn n_Lhc_Num_Num tc_Foreign_C_Types_CTime ]

constantMethods = [
   (n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Lhc.Basics.Integer", ELam (v0 t_Lhc_Basics_Integer) $ prim_sizeof "bits<max>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Lhc.Basics.Integer", buildPoke dc_Integer t_Lhc_Basics_Integer "bits<max>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Lhc.Basics.Integer", buildPeek dc_Integer t_Lhc_Basics_Integer "bits<max>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Lhc.Basics.Integer", prim_maxbound dc_Integer t_Lhc_Basics_Integer "bits<max>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Lhc.Basics.Integer", prim_minbound dc_Integer t_Lhc_Basics_Integer "bits<max>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Lhc.Basics.Integer", ELam v2_Int (create_integralCast_fromInt dc_Integer r_bits_max_ (EVar v2_Int) t_Lhc_Basics_Integer))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Lhc.Basics.Integer", ELam (v2 t_Lhc_Basics_Integer) (create_integralCast_toInt dc_Integer r_bits_max_ (EVar (v2 t_Lhc_Basics_Integer))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Lhc.Basics.Integer", ELam v2_Integer (EVar v2_Integer))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Lhc.Basics.Integer", ELam v2_Integer (EVar v2_Integer))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Lhc.Basics.Integer", ELam (v2 t_Lhc_Basics_Integer) (build_abs "bits<max>" dc_Integer (EVar (v2 t_Lhc_Basics_Integer))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Lhc.Basics.Integer", ELam (v2 t_Lhc_Basics_Integer) (build_signum "bits<max>" dc_Integer (EVar (v2 t_Lhc_Basics_Integer)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Lhc.Basics.Integer", op_aaB  Op.Eq "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Lhc.Basics.Integer", op_aaB  Op.Gte "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Lhc.Basics.Integer", op_aaB  Op.Lte "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Lhc.Basics.Integer", op_aaB  Op.Gt "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Lhc.Basics.Integer", op_aaB  Op.Lt "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Lhc.Basics.Integer", op_aaa  Op.Add "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Lhc.Basics.Integer", op_aaa  Op.Sub "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Lhc.Basics.Integer", op_aaa  Op.Mul "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Lhc.Basics.Integer", op_aa  Op.Neg "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Lhc.Basics.Integer", op_aaa  Op.And "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Lhc.Basics.Integer", op_aaa  Op.Or "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Lhc.Basics.Integer", op_aaa  Op.Xor "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Lhc.Basics.Integer", op_aa  Op.Com "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Lhc.Basics.Integer", op_aaa  Op.Div "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Lhc.Basics.Integer", op_aaa  Op.Mod "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Lhc.Basics.Integer", op_aaa  Op.Div "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Lhc.Basics.Integer", op_aaa  Op.Mod "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Lhc.Basics.Integer", op_aIa  Op.Shl "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Lhc.Basics.Integer", op_aIa  Op.Shra "bits<max>" dc_Integer t_Lhc_Basics_Integer)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Int.Int8", ELam (v0 t_Data_Int_Int8) $ prim_sizeof "bits8")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Int.Int8", buildPoke dc_Int8 t_Data_Int_Int8 "bits8")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Int.Int8", buildPeek dc_Int8 t_Data_Int_Int8 "bits8")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Int.Int8", prim_maxbound dc_Int8 t_Data_Int_Int8 "bits8")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Int.Int8", prim_minbound dc_Int8 t_Data_Int_Int8 "bits8")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Int.Int8", ELam v2_Int (create_integralCast_fromInt dc_Int8 r_bits8 (EVar v2_Int) t_Data_Int_Int8))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Int.Int8", ELam (v2 t_Data_Int_Int8) (create_integralCast_toInt dc_Int8 r_bits8 (EVar (v2 t_Data_Int_Int8))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Int.Int8", ELam v2_Integer (create_integralCast_fromInteger dc_Int8 r_bits8 (EVar v2_Integer) t_Data_Int_Int8))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Int.Int8", ELam (v2 t_Data_Int_Int8) (create_integralCast_toInteger dc_Int8 r_bits8 (EVar (v2 t_Data_Int_Int8))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Int.Int8", ELam (v2 t_Data_Int_Int8) (build_abs "bits8" dc_Int8 (EVar (v2 t_Data_Int_Int8))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Int.Int8", ELam (v2 t_Data_Int_Int8) (build_signum "bits8" dc_Int8 (EVar (v2 t_Data_Int_Int8)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Int.Int8", op_aaB  Op.Eq "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Int.Int8", op_aaB  Op.Gte "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Int.Int8", op_aaB  Op.Lte "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Int.Int8", op_aaB  Op.Gt "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Int.Int8", op_aaB  Op.Lt "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Int.Int8", op_aaa  Op.Add "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Int.Int8", op_aaa  Op.Sub "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Int.Int8", op_aaa  Op.Mul "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Int.Int8", op_aa  Op.Neg "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Int.Int8", op_aaa  Op.And "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Int.Int8", op_aaa  Op.Or "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Int.Int8", op_aaa  Op.Xor "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Int.Int8", op_aa  Op.Com "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Int.Int8", op_aaa  Op.Div "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Int.Int8", op_aaa  Op.Mod "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Int.Int8", op_aaa  Op.Div "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Int.Int8", op_aaa  Op.Mod "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Int.Int8", op_aIa  Op.Shl "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Int.Int8", op_aIa  Op.Shra "bits8" dc_Int8 t_Data_Int_Int8)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Int.Int16", ELam (v0 t_Data_Int_Int16) $ prim_sizeof "bits16")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Int.Int16", buildPoke dc_Int16 t_Data_Int_Int16 "bits16")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Int.Int16", buildPeek dc_Int16 t_Data_Int_Int16 "bits16")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Int.Int16", prim_maxbound dc_Int16 t_Data_Int_Int16 "bits16")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Int.Int16", prim_minbound dc_Int16 t_Data_Int_Int16 "bits16")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Int.Int16", ELam v2_Int (create_integralCast_fromInt dc_Int16 r_bits16 (EVar v2_Int) t_Data_Int_Int16))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Int.Int16", ELam (v2 t_Data_Int_Int16) (create_integralCast_toInt dc_Int16 r_bits16 (EVar (v2 t_Data_Int_Int16))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Int.Int16", ELam v2_Integer (create_integralCast_fromInteger dc_Int16 r_bits16 (EVar v2_Integer) t_Data_Int_Int16))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Int.Int16", ELam (v2 t_Data_Int_Int16) (create_integralCast_toInteger dc_Int16 r_bits16 (EVar (v2 t_Data_Int_Int16))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Int.Int16", ELam (v2 t_Data_Int_Int16) (build_abs "bits16" dc_Int16 (EVar (v2 t_Data_Int_Int16))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Int.Int16", ELam (v2 t_Data_Int_Int16) (build_signum "bits16" dc_Int16 (EVar (v2 t_Data_Int_Int16)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Int.Int16", op_aaB  Op.Eq "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Int.Int16", op_aaB  Op.Gte "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Int.Int16", op_aaB  Op.Lte "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Int.Int16", op_aaB  Op.Gt "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Int.Int16", op_aaB  Op.Lt "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Int.Int16", op_aaa  Op.Add "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Int.Int16", op_aaa  Op.Sub "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Int.Int16", op_aaa  Op.Mul "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Int.Int16", op_aa  Op.Neg "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Int.Int16", op_aaa  Op.And "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Int.Int16", op_aaa  Op.Or "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Int.Int16", op_aaa  Op.Xor "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Int.Int16", op_aa  Op.Com "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Int.Int16", op_aaa  Op.Div "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Int.Int16", op_aaa  Op.Mod "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Int.Int16", op_aaa  Op.Div "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Int.Int16", op_aaa  Op.Mod "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Int.Int16", op_aIa  Op.Shl "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Int.Int16", op_aIa  Op.Shra "bits16" dc_Int16 t_Data_Int_Int16)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Int.Int32", ELam (v0 t_Data_Int_Int32) $ prim_sizeof "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Int.Int32", buildPoke dc_Int32 t_Data_Int_Int32 "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Int.Int32", buildPeek dc_Int32 t_Data_Int_Int32 "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Int.Int32", prim_maxbound dc_Int32 t_Data_Int_Int32 "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Int.Int32", prim_minbound dc_Int32 t_Data_Int_Int32 "bits32")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Int.Int32", ELam v2_Int (create_integralCast_fromInt dc_Int32 r_bits32 (EVar v2_Int) t_Data_Int_Int32))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Int.Int32", ELam (v2 t_Data_Int_Int32) (create_integralCast_toInt dc_Int32 r_bits32 (EVar (v2 t_Data_Int_Int32))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Int.Int32", ELam v2_Integer (create_integralCast_fromInteger dc_Int32 r_bits32 (EVar v2_Integer) t_Data_Int_Int32))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Int.Int32", ELam (v2 t_Data_Int_Int32) (create_integralCast_toInteger dc_Int32 r_bits32 (EVar (v2 t_Data_Int_Int32))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Int.Int32", ELam (v2 t_Data_Int_Int32) (build_abs "bits32" dc_Int32 (EVar (v2 t_Data_Int_Int32))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Int.Int32", ELam (v2 t_Data_Int_Int32) (build_signum "bits32" dc_Int32 (EVar (v2 t_Data_Int_Int32)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Int.Int32", op_aaB  Op.Eq "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Int.Int32", op_aaB  Op.Gte "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Int.Int32", op_aaB  Op.Lte "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Int.Int32", op_aaB  Op.Gt "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Int.Int32", op_aaB  Op.Lt "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Int.Int32", op_aaa  Op.Add "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Int.Int32", op_aaa  Op.Sub "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Int.Int32", op_aaa  Op.Mul "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Int.Int32", op_aa  Op.Neg "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Int.Int32", op_aaa  Op.And "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Int.Int32", op_aaa  Op.Or "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Int.Int32", op_aaa  Op.Xor "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Int.Int32", op_aa  Op.Com "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Int.Int32", op_aaa  Op.Div "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Int.Int32", op_aaa  Op.Mod "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Int.Int32", op_aaa  Op.Div "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Int.Int32", op_aaa  Op.Mod "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Int.Int32", op_aIa  Op.Shl "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Int.Int32", op_aIa  Op.Shra "bits32" dc_Int32 t_Data_Int_Int32)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Int.Int64", ELam (v0 t_Data_Int_Int64) $ prim_sizeof "bits64")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Int.Int64", buildPoke dc_Int64 t_Data_Int_Int64 "bits64")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Int.Int64", buildPeek dc_Int64 t_Data_Int_Int64 "bits64")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Int.Int64", prim_maxbound dc_Int64 t_Data_Int_Int64 "bits64")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Int.Int64", prim_minbound dc_Int64 t_Data_Int_Int64 "bits64")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Int.Int64", ELam v2_Int (create_integralCast_fromInt dc_Int64 r_bits64 (EVar v2_Int) t_Data_Int_Int64))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Int.Int64", ELam (v2 t_Data_Int_Int64) (create_integralCast_toInt dc_Int64 r_bits64 (EVar (v2 t_Data_Int_Int64))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Int.Int64", ELam v2_Integer (create_integralCast_fromInteger dc_Int64 r_bits64 (EVar v2_Integer) t_Data_Int_Int64))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Int.Int64", ELam (v2 t_Data_Int_Int64) (create_integralCast_toInteger dc_Int64 r_bits64 (EVar (v2 t_Data_Int_Int64))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Int.Int64", ELam (v2 t_Data_Int_Int64) (build_abs "bits64" dc_Int64 (EVar (v2 t_Data_Int_Int64))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Int.Int64", ELam (v2 t_Data_Int_Int64) (build_signum "bits64" dc_Int64 (EVar (v2 t_Data_Int_Int64)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Int.Int64", op_aaB  Op.Eq "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Int.Int64", op_aaB  Op.Gte "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Int.Int64", op_aaB  Op.Lte "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Int.Int64", op_aaB  Op.Gt "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Int.Int64", op_aaB  Op.Lt "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Int.Int64", op_aaa  Op.Add "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Int.Int64", op_aaa  Op.Sub "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Int.Int64", op_aaa  Op.Mul "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Int.Int64", op_aa  Op.Neg "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Int.Int64", op_aaa  Op.And "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Int.Int64", op_aaa  Op.Or "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Int.Int64", op_aaa  Op.Xor "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Int.Int64", op_aa  Op.Com "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Int.Int64", op_aaa  Op.Div "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Int.Int64", op_aaa  Op.Mod "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Int.Int64", op_aaa  Op.Div "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Int.Int64", op_aaa  Op.Mod "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Int.Int64", op_aIa  Op.Shl "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Int.Int64", op_aIa  Op.Shra "bits64" dc_Int64 t_Data_Int_Int64)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Int.IntMax", ELam (v0 t_Data_Int_IntMax) $ prim_sizeof "bits<max>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Int.IntMax", buildPoke dc_IntMax t_Data_Int_IntMax "bits<max>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Int.IntMax", buildPeek dc_IntMax t_Data_Int_IntMax "bits<max>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Int.IntMax", prim_maxbound dc_IntMax t_Data_Int_IntMax "bits<max>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Int.IntMax", prim_minbound dc_IntMax t_Data_Int_IntMax "bits<max>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Int.IntMax", ELam v2_Int (create_integralCast_fromInt dc_IntMax r_bits_max_ (EVar v2_Int) t_Data_Int_IntMax))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Int.IntMax", ELam (v2 t_Data_Int_IntMax) (create_integralCast_toInt dc_IntMax r_bits_max_ (EVar (v2 t_Data_Int_IntMax))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Int.IntMax", ELam v2_Integer (create_integralCast_fromInteger dc_IntMax r_bits_max_ (EVar v2_Integer) t_Data_Int_IntMax))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Int.IntMax", ELam (v2 t_Data_Int_IntMax) (create_integralCast_toInteger dc_IntMax r_bits_max_ (EVar (v2 t_Data_Int_IntMax))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Int.IntMax", ELam (v2 t_Data_Int_IntMax) (build_abs "bits<max>" dc_IntMax (EVar (v2 t_Data_Int_IntMax))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Int.IntMax", ELam (v2 t_Data_Int_IntMax) (build_signum "bits<max>" dc_IntMax (EVar (v2 t_Data_Int_IntMax)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Int.IntMax", op_aaB  Op.Eq "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Int.IntMax", op_aaB  Op.Gte "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Int.IntMax", op_aaB  Op.Lte "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Int.IntMax", op_aaB  Op.Gt "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Int.IntMax", op_aaB  Op.Lt "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Int.IntMax", op_aaa  Op.Add "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Int.IntMax", op_aaa  Op.Sub "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Int.IntMax", op_aaa  Op.Mul "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Int.IntMax", op_aa  Op.Neg "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Int.IntMax", op_aaa  Op.And "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Int.IntMax", op_aaa  Op.Or "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Int.IntMax", op_aaa  Op.Xor "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Int.IntMax", op_aa  Op.Com "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Int.IntMax", op_aaa  Op.Div "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Int.IntMax", op_aaa  Op.Mod "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Int.IntMax", op_aaa  Op.Div "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Int.IntMax", op_aaa  Op.Mod "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Int.IntMax", op_aIa  Op.Shl "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Int.IntMax", op_aIa  Op.Shra "bits<max>" dc_IntMax t_Data_Int_IntMax)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Int.IntPtr", ELam (v0 t_Data_Int_IntPtr) $ prim_sizeof "bits<ptr>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Int.IntPtr", buildPoke dc_IntPtr t_Data_Int_IntPtr "bits<ptr>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Int.IntPtr", buildPeek dc_IntPtr t_Data_Int_IntPtr "bits<ptr>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Int.IntPtr", prim_maxbound dc_IntPtr t_Data_Int_IntPtr "bits<ptr>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Int.IntPtr", prim_minbound dc_IntPtr t_Data_Int_IntPtr "bits<ptr>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Int.IntPtr", ELam v2_Int (create_integralCast_fromInt dc_IntPtr r_bits_ptr_ (EVar v2_Int) t_Data_Int_IntPtr))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Int.IntPtr", ELam (v2 t_Data_Int_IntPtr) (create_integralCast_toInt dc_IntPtr r_bits_ptr_ (EVar (v2 t_Data_Int_IntPtr))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Int.IntPtr", ELam v2_Integer (create_integralCast_fromInteger dc_IntPtr r_bits_ptr_ (EVar v2_Integer) t_Data_Int_IntPtr))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Int.IntPtr", ELam (v2 t_Data_Int_IntPtr) (create_integralCast_toInteger dc_IntPtr r_bits_ptr_ (EVar (v2 t_Data_Int_IntPtr))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Int.IntPtr", ELam (v2 t_Data_Int_IntPtr) (build_abs "bits<ptr>" dc_IntPtr (EVar (v2 t_Data_Int_IntPtr))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Int.IntPtr", ELam (v2 t_Data_Int_IntPtr) (build_signum "bits<ptr>" dc_IntPtr (EVar (v2 t_Data_Int_IntPtr)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Int.IntPtr", op_aaB  Op.Eq "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Int.IntPtr", op_aaB  Op.Gte "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Int.IntPtr", op_aaB  Op.Lte "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Int.IntPtr", op_aaB  Op.Gt "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Int.IntPtr", op_aaB  Op.Lt "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Int.IntPtr", op_aaa  Op.Add "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Int.IntPtr", op_aaa  Op.Sub "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Int.IntPtr", op_aaa  Op.Mul "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Int.IntPtr", op_aa  Op.Neg "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Int.IntPtr", op_aaa  Op.And "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Int.IntPtr", op_aaa  Op.Or "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Int.IntPtr", op_aaa  Op.Xor "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Int.IntPtr", op_aa  Op.Com "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Int.IntPtr", op_aaa  Op.Div "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Int.IntPtr", op_aaa  Op.Mod "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Int.IntPtr", op_aaa  Op.Div "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Int.IntPtr", op_aaa  Op.Mod "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Int.IntPtr", op_aIa  Op.Shl "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Int.IntPtr", op_aIa  Op.Shra "bits<ptr>" dc_IntPtr t_Data_Int_IntPtr)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Word.Word", ELam (v0 t_Data_Word_Word) $ prim_sizeof "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Word.Word", buildPoke dc_Word t_Data_Word_Word "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Word.Word", buildPeek dc_Word t_Data_Word_Word "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Word.Word", prim_umaxbound dc_Word t_Data_Word_Word "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Word.Word", prim_uminbound dc_Word t_Data_Word_Word "bits32")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Word.Word", ELam v2_Int (create_uintegralCast_fromInt dc_Word r_bits32 (EVar v2_Int) t_Data_Word_Word))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Word.Word", ELam (v2 t_Data_Word_Word) (create_uintegralCast_toInt dc_Word r_bits32 (EVar (v2 t_Data_Word_Word))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Word.Word", ELam v2_Integer (create_uintegralCast_fromInteger dc_Word r_bits32 (EVar v2_Integer) t_Data_Word_Word))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Word.Word", ELam (v2 t_Data_Word_Word) (create_uintegralCast_toInteger dc_Word r_bits32 (EVar (v2 t_Data_Word_Word))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Word.Word", ELam (v2 t_Data_Word_Word) (build_uabs "bits32" dc_Word (EVar (v2 t_Data_Word_Word))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Word.Word", ELam (v2 t_Data_Word_Word) (build_usignum "bits32" dc_Word (EVar (v2 t_Data_Word_Word)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Word.Word", op_aaB  Op.Eq "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Word.Word", op_aaB  Op.UGte "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Word.Word", op_aaB  Op.ULte "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Word.Word", op_aaB  Op.UGt "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Word.Word", op_aaB  Op.ULt "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Word.Word", op_aaa  Op.Add "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Word.Word", op_aaa  Op.Sub "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Word.Word", op_aaa  Op.Mul "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Word.Word", op_aa  Op.Neg "bits32" dc_Word t_Data_Word_Word)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Word.Word", op_aaa  Op.And "bits32" dc_Word t_Data_Word_Word)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Word.Word", op_aaa  Op.Or "bits32" dc_Word t_Data_Word_Word)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Word.Word", op_aaa  Op.Xor "bits32" dc_Word t_Data_Word_Word)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Word.Word", op_aa  Op.Com "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Word.Word", op_aaa  Op.UDiv "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Word.Word", op_aaa  Op.UMod "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Word.Word", op_aaa  Op.UDiv "bits32" dc_Word t_Data_Word_Word)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Word.Word", op_aaa  Op.UMod "bits32" dc_Word t_Data_Word_Word)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Word.Word", op_aIa  Op.Shl "bits32" dc_Word t_Data_Word_Word)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Word.Word", op_aIa  Op.Shr "bits32" dc_Word t_Data_Word_Word)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Word.Word8", ELam (v0 t_Data_Word_Word8) $ prim_sizeof "bits8")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Word.Word8", buildPoke dc_Word8 t_Data_Word_Word8 "bits8")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Word.Word8", buildPeek dc_Word8 t_Data_Word_Word8 "bits8")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Word.Word8", prim_umaxbound dc_Word8 t_Data_Word_Word8 "bits8")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Word.Word8", prim_uminbound dc_Word8 t_Data_Word_Word8 "bits8")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Word.Word8", ELam v2_Int (create_uintegralCast_fromInt dc_Word8 r_bits8 (EVar v2_Int) t_Data_Word_Word8))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Word.Word8", ELam (v2 t_Data_Word_Word8) (create_uintegralCast_toInt dc_Word8 r_bits8 (EVar (v2 t_Data_Word_Word8))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Word.Word8", ELam v2_Integer (create_uintegralCast_fromInteger dc_Word8 r_bits8 (EVar v2_Integer) t_Data_Word_Word8))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Word.Word8", ELam (v2 t_Data_Word_Word8) (create_uintegralCast_toInteger dc_Word8 r_bits8 (EVar (v2 t_Data_Word_Word8))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Word.Word8", ELam (v2 t_Data_Word_Word8) (build_uabs "bits8" dc_Word8 (EVar (v2 t_Data_Word_Word8))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Word.Word8", ELam (v2 t_Data_Word_Word8) (build_usignum "bits8" dc_Word8 (EVar (v2 t_Data_Word_Word8)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Word.Word8", op_aaB  Op.Eq "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Word.Word8", op_aaB  Op.UGte "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Word.Word8", op_aaB  Op.ULte "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Word.Word8", op_aaB  Op.UGt "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Word.Word8", op_aaB  Op.ULt "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Word.Word8", op_aaa  Op.Add "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Word.Word8", op_aaa  Op.Sub "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Word.Word8", op_aaa  Op.Mul "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Word.Word8", op_aa  Op.Neg "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Word.Word8", op_aaa  Op.And "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Word.Word8", op_aaa  Op.Or "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Word.Word8", op_aaa  Op.Xor "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Word.Word8", op_aa  Op.Com "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Word.Word8", op_aaa  Op.UDiv "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Word.Word8", op_aaa  Op.UMod "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Word.Word8", op_aaa  Op.UDiv "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Word.Word8", op_aaa  Op.UMod "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Word.Word8", op_aIa  Op.Shl "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Word.Word8", op_aIa  Op.Shr "bits8" dc_Word8 t_Data_Word_Word8)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Word.Word16", ELam (v0 t_Data_Word_Word16) $ prim_sizeof "bits16")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Word.Word16", buildPoke dc_Word16 t_Data_Word_Word16 "bits16")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Word.Word16", buildPeek dc_Word16 t_Data_Word_Word16 "bits16")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Word.Word16", prim_umaxbound dc_Word16 t_Data_Word_Word16 "bits16")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Word.Word16", prim_uminbound dc_Word16 t_Data_Word_Word16 "bits16")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Word.Word16", ELam v2_Int (create_uintegralCast_fromInt dc_Word16 r_bits16 (EVar v2_Int) t_Data_Word_Word16))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Word.Word16", ELam (v2 t_Data_Word_Word16) (create_uintegralCast_toInt dc_Word16 r_bits16 (EVar (v2 t_Data_Word_Word16))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Word.Word16", ELam v2_Integer (create_uintegralCast_fromInteger dc_Word16 r_bits16 (EVar v2_Integer) t_Data_Word_Word16))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Word.Word16", ELam (v2 t_Data_Word_Word16) (create_uintegralCast_toInteger dc_Word16 r_bits16 (EVar (v2 t_Data_Word_Word16))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Word.Word16", ELam (v2 t_Data_Word_Word16) (build_uabs "bits16" dc_Word16 (EVar (v2 t_Data_Word_Word16))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Word.Word16", ELam (v2 t_Data_Word_Word16) (build_usignum "bits16" dc_Word16 (EVar (v2 t_Data_Word_Word16)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Word.Word16", op_aaB  Op.Eq "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Word.Word16", op_aaB  Op.UGte "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Word.Word16", op_aaB  Op.ULte "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Word.Word16", op_aaB  Op.UGt "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Word.Word16", op_aaB  Op.ULt "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Word.Word16", op_aaa  Op.Add "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Word.Word16", op_aaa  Op.Sub "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Word.Word16", op_aaa  Op.Mul "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Word.Word16", op_aa  Op.Neg "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Word.Word16", op_aaa  Op.And "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Word.Word16", op_aaa  Op.Or "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Word.Word16", op_aaa  Op.Xor "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Word.Word16", op_aa  Op.Com "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Word.Word16", op_aaa  Op.UDiv "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Word.Word16", op_aaa  Op.UMod "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Word.Word16", op_aaa  Op.UDiv "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Word.Word16", op_aaa  Op.UMod "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Word.Word16", op_aIa  Op.Shl "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Word.Word16", op_aIa  Op.Shr "bits16" dc_Word16 t_Data_Word_Word16)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Word.Word32", ELam (v0 t_Data_Word_Word32) $ prim_sizeof "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Word.Word32", buildPoke dc_Word32 t_Data_Word_Word32 "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Word.Word32", buildPeek dc_Word32 t_Data_Word_Word32 "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Word.Word32", prim_umaxbound dc_Word32 t_Data_Word_Word32 "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Word.Word32", prim_uminbound dc_Word32 t_Data_Word_Word32 "bits32")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Word.Word32", ELam v2_Int (create_uintegralCast_fromInt dc_Word32 r_bits32 (EVar v2_Int) t_Data_Word_Word32))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Word.Word32", ELam (v2 t_Data_Word_Word32) (create_uintegralCast_toInt dc_Word32 r_bits32 (EVar (v2 t_Data_Word_Word32))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Word.Word32", ELam v2_Integer (create_uintegralCast_fromInteger dc_Word32 r_bits32 (EVar v2_Integer) t_Data_Word_Word32))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Word.Word32", ELam (v2 t_Data_Word_Word32) (create_uintegralCast_toInteger dc_Word32 r_bits32 (EVar (v2 t_Data_Word_Word32))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Word.Word32", ELam (v2 t_Data_Word_Word32) (build_uabs "bits32" dc_Word32 (EVar (v2 t_Data_Word_Word32))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Word.Word32", ELam (v2 t_Data_Word_Word32) (build_usignum "bits32" dc_Word32 (EVar (v2 t_Data_Word_Word32)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Word.Word32", op_aaB  Op.Eq "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Word.Word32", op_aaB  Op.UGte "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Word.Word32", op_aaB  Op.ULte "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Word.Word32", op_aaB  Op.UGt "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Word.Word32", op_aaB  Op.ULt "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Word.Word32", op_aaa  Op.Add "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Word.Word32", op_aaa  Op.Sub "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Word.Word32", op_aaa  Op.Mul "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Word.Word32", op_aa  Op.Neg "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Word.Word32", op_aaa  Op.And "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Word.Word32", op_aaa  Op.Or "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Word.Word32", op_aaa  Op.Xor "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Word.Word32", op_aa  Op.Com "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Word.Word32", op_aaa  Op.UDiv "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Word.Word32", op_aaa  Op.UMod "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Word.Word32", op_aaa  Op.UDiv "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Word.Word32", op_aaa  Op.UMod "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Word.Word32", op_aIa  Op.Shl "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Word.Word32", op_aIa  Op.Shr "bits32" dc_Word32 t_Data_Word_Word32)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Word.Word64", ELam (v0 t_Data_Word_Word64) $ prim_sizeof "bits64")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Word.Word64", buildPoke dc_Word64 t_Data_Word_Word64 "bits64")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Word.Word64", buildPeek dc_Word64 t_Data_Word_Word64 "bits64")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Word.Word64", prim_umaxbound dc_Word64 t_Data_Word_Word64 "bits64")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Word.Word64", prim_uminbound dc_Word64 t_Data_Word_Word64 "bits64")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Word.Word64", ELam v2_Int (create_uintegralCast_fromInt dc_Word64 r_bits64 (EVar v2_Int) t_Data_Word_Word64))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Word.Word64", ELam (v2 t_Data_Word_Word64) (create_uintegralCast_toInt dc_Word64 r_bits64 (EVar (v2 t_Data_Word_Word64))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Word.Word64", ELam v2_Integer (create_uintegralCast_fromInteger dc_Word64 r_bits64 (EVar v2_Integer) t_Data_Word_Word64))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Word.Word64", ELam (v2 t_Data_Word_Word64) (create_uintegralCast_toInteger dc_Word64 r_bits64 (EVar (v2 t_Data_Word_Word64))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Word.Word64", ELam (v2 t_Data_Word_Word64) (build_uabs "bits64" dc_Word64 (EVar (v2 t_Data_Word_Word64))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Word.Word64", ELam (v2 t_Data_Word_Word64) (build_usignum "bits64" dc_Word64 (EVar (v2 t_Data_Word_Word64)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Word.Word64", op_aaB  Op.Eq "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Word.Word64", op_aaB  Op.UGte "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Word.Word64", op_aaB  Op.ULte "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Word.Word64", op_aaB  Op.UGt "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Word.Word64", op_aaB  Op.ULt "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Word.Word64", op_aaa  Op.Add "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Word.Word64", op_aaa  Op.Sub "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Word.Word64", op_aaa  Op.Mul "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Word.Word64", op_aa  Op.Neg "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Word.Word64", op_aaa  Op.And "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Word.Word64", op_aaa  Op.Or "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Word.Word64", op_aaa  Op.Xor "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Word.Word64", op_aa  Op.Com "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Word.Word64", op_aaa  Op.UDiv "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Word.Word64", op_aaa  Op.UMod "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Word.Word64", op_aaa  Op.UDiv "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Word.Word64", op_aaa  Op.UMod "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Word.Word64", op_aIa  Op.Shl "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Word.Word64", op_aIa  Op.Shr "bits64" dc_Word64 t_Data_Word_Word64)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Word.WordMax", ELam (v0 t_Data_Word_WordMax) $ prim_sizeof "bits<max>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Word.WordMax", buildPoke dc_WordMax t_Data_Word_WordMax "bits<max>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Word.WordMax", buildPeek dc_WordMax t_Data_Word_WordMax "bits<max>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Word.WordMax", prim_umaxbound dc_WordMax t_Data_Word_WordMax "bits<max>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Word.WordMax", prim_uminbound dc_WordMax t_Data_Word_WordMax "bits<max>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Word.WordMax", ELam v2_Int (create_uintegralCast_fromInt dc_WordMax r_bits_max_ (EVar v2_Int) t_Data_Word_WordMax))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Word.WordMax", ELam (v2 t_Data_Word_WordMax) (create_uintegralCast_toInt dc_WordMax r_bits_max_ (EVar (v2 t_Data_Word_WordMax))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Word.WordMax", ELam v2_Integer (create_uintegralCast_fromInteger dc_WordMax r_bits_max_ (EVar v2_Integer) t_Data_Word_WordMax))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Word.WordMax", ELam (v2 t_Data_Word_WordMax) (create_uintegralCast_toInteger dc_WordMax r_bits_max_ (EVar (v2 t_Data_Word_WordMax))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Word.WordMax", ELam (v2 t_Data_Word_WordMax) (build_uabs "bits<max>" dc_WordMax (EVar (v2 t_Data_Word_WordMax))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Word.WordMax", ELam (v2 t_Data_Word_WordMax) (build_usignum "bits<max>" dc_WordMax (EVar (v2 t_Data_Word_WordMax)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Word.WordMax", op_aaB  Op.Eq "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Word.WordMax", op_aaB  Op.UGte "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Word.WordMax", op_aaB  Op.ULte "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Word.WordMax", op_aaB  Op.UGt "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Word.WordMax", op_aaB  Op.ULt "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Word.WordMax", op_aaa  Op.Add "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Word.WordMax", op_aaa  Op.Sub "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Word.WordMax", op_aaa  Op.Mul "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Word.WordMax", op_aa  Op.Neg "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Word.WordMax", op_aaa  Op.And "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Word.WordMax", op_aaa  Op.Or "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Word.WordMax", op_aaa  Op.Xor "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Word.WordMax", op_aa  Op.Com "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Word.WordMax", op_aaa  Op.UDiv "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Word.WordMax", op_aaa  Op.UMod "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Word.WordMax", op_aaa  Op.UDiv "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Word.WordMax", op_aaa  Op.UMod "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Word.WordMax", op_aIa  Op.Shl "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Word.WordMax", op_aIa  Op.Shr "bits<max>" dc_WordMax t_Data_Word_WordMax)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Data.Word.WordPtr", ELam (v0 t_Data_Word_WordPtr) $ prim_sizeof "bits<ptr>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Data.Word.WordPtr", buildPoke dc_WordPtr t_Data_Word_WordPtr "bits<ptr>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Data.Word.WordPtr", buildPeek dc_WordPtr t_Data_Word_WordPtr "bits<ptr>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Data.Word.WordPtr", prim_umaxbound dc_WordPtr t_Data_Word_WordPtr "bits<ptr>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Data.Word.WordPtr", prim_uminbound dc_WordPtr t_Data_Word_WordPtr "bits<ptr>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Data.Word.WordPtr", ELam v2_Int (create_uintegralCast_fromInt dc_WordPtr r_bits_ptr_ (EVar v2_Int) t_Data_Word_WordPtr))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Data.Word.WordPtr", ELam (v2 t_Data_Word_WordPtr) (create_uintegralCast_toInt dc_WordPtr r_bits_ptr_ (EVar (v2 t_Data_Word_WordPtr))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Data.Word.WordPtr", ELam v2_Integer (create_uintegralCast_fromInteger dc_WordPtr r_bits_ptr_ (EVar v2_Integer) t_Data_Word_WordPtr))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Data.Word.WordPtr", ELam (v2 t_Data_Word_WordPtr) (create_uintegralCast_toInteger dc_WordPtr r_bits_ptr_ (EVar (v2 t_Data_Word_WordPtr))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Data.Word.WordPtr", ELam (v2 t_Data_Word_WordPtr) (build_uabs "bits<ptr>" dc_WordPtr (EVar (v2 t_Data_Word_WordPtr))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Data.Word.WordPtr", ELam (v2 t_Data_Word_WordPtr) (build_usignum "bits<ptr>" dc_WordPtr (EVar (v2 t_Data_Word_WordPtr)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Data.Word.WordPtr", op_aaB  Op.Eq "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Data.Word.WordPtr", op_aaB  Op.UGte "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Data.Word.WordPtr", op_aaB  Op.ULte "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Data.Word.WordPtr", op_aaB  Op.UGt "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Data.Word.WordPtr", op_aaB  Op.ULt "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Data.Word.WordPtr", op_aaa  Op.Add "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Data.Word.WordPtr", op_aaa  Op.Sub "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Data.Word.WordPtr", op_aaa  Op.Mul "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Data.Word.WordPtr", op_aa  Op.Neg "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Data.Word.WordPtr", op_aaa  Op.And "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Data.Word.WordPtr", op_aaa  Op.Or "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Data.Word.WordPtr", op_aaa  Op.Xor "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Data.Word.WordPtr", op_aa  Op.Com "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Data.Word.WordPtr", op_aaa  Op.UDiv "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Data.Word.WordPtr", op_aaa  Op.UMod "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Data.Word.WordPtr", op_aaa  Op.UDiv "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Data.Word.WordPtr", op_aaa  Op.UMod "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Data.Word.WordPtr", op_aIa  Op.Shl "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Data.Word.WordPtr", op_aIa  Op.Shr "bits<ptr>" dc_WordPtr t_Data_Word_WordPtr)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Foreign.C.Types.CChar", ELam (v0 t_Foreign_C_Types_CChar) $ prim_sizeof "bits8")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Foreign.C.Types.CChar", buildPoke dc_CChar t_Foreign_C_Types_CChar "bits8")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Foreign.C.Types.CChar", buildPeek dc_CChar t_Foreign_C_Types_CChar "bits8")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Foreign.C.Types.CChar", prim_maxbound dc_CChar t_Foreign_C_Types_CChar "bits8")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Foreign.C.Types.CChar", prim_minbound dc_CChar t_Foreign_C_Types_CChar "bits8")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Foreign.C.Types.CChar", ELam v2_Int (create_integralCast_fromInt dc_CChar r_bits8 (EVar v2_Int) t_Foreign_C_Types_CChar))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Foreign.C.Types.CChar", ELam (v2 t_Foreign_C_Types_CChar) (create_integralCast_toInt dc_CChar r_bits8 (EVar (v2 t_Foreign_C_Types_CChar))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Foreign.C.Types.CChar", ELam v2_Integer (create_integralCast_fromInteger dc_CChar r_bits8 (EVar v2_Integer) t_Foreign_C_Types_CChar))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Foreign.C.Types.CChar", ELam (v2 t_Foreign_C_Types_CChar) (create_integralCast_toInteger dc_CChar r_bits8 (EVar (v2 t_Foreign_C_Types_CChar))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Foreign.C.Types.CChar", ELam (v2 t_Foreign_C_Types_CChar) (build_abs "bits8" dc_CChar (EVar (v2 t_Foreign_C_Types_CChar))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Foreign.C.Types.CChar", ELam (v2 t_Foreign_C_Types_CChar) (build_signum "bits8" dc_CChar (EVar (v2 t_Foreign_C_Types_CChar)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Foreign.C.Types.CChar", op_aaB  Op.Eq "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Foreign.C.Types.CChar", op_aaB  Op.Gte "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Foreign.C.Types.CChar", op_aaB  Op.Lte "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Foreign.C.Types.CChar", op_aaB  Op.Gt "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Foreign.C.Types.CChar", op_aaB  Op.Lt "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Foreign.C.Types.CChar", op_aaa  Op.Add "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Foreign.C.Types.CChar", op_aaa  Op.Sub "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Foreign.C.Types.CChar", op_aaa  Op.Mul "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Foreign.C.Types.CChar", op_aa  Op.Neg "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Foreign.C.Types.CChar", op_aaa  Op.And "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Foreign.C.Types.CChar", op_aaa  Op.Or "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Foreign.C.Types.CChar", op_aaa  Op.Xor "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Foreign.C.Types.CChar", op_aa  Op.Com "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Foreign.C.Types.CChar", op_aaa  Op.Div "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Foreign.C.Types.CChar", op_aaa  Op.Mod "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Foreign.C.Types.CChar", op_aaa  Op.Div "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Foreign.C.Types.CChar", op_aaa  Op.Mod "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Foreign.C.Types.CChar", op_aIa  Op.Shl "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Foreign.C.Types.CChar", op_aIa  Op.Shra "bits8" dc_CChar t_Foreign_C_Types_CChar)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Foreign.C.Types.CShort", ELam (v0 t_Foreign_C_Types_CShort) $ prim_sizeof "bits<short>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Foreign.C.Types.CShort", buildPoke dc_CShort t_Foreign_C_Types_CShort "bits<short>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Foreign.C.Types.CShort", buildPeek dc_CShort t_Foreign_C_Types_CShort "bits<short>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Foreign.C.Types.CShort", prim_maxbound dc_CShort t_Foreign_C_Types_CShort "bits<short>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Foreign.C.Types.CShort", prim_minbound dc_CShort t_Foreign_C_Types_CShort "bits<short>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Foreign.C.Types.CShort", ELam v2_Int (create_integralCast_fromInt dc_CShort r_bits_short_ (EVar v2_Int) t_Foreign_C_Types_CShort))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Foreign.C.Types.CShort", ELam (v2 t_Foreign_C_Types_CShort) (create_integralCast_toInt dc_CShort r_bits_short_ (EVar (v2 t_Foreign_C_Types_CShort))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Foreign.C.Types.CShort", ELam v2_Integer (create_integralCast_fromInteger dc_CShort r_bits_short_ (EVar v2_Integer) t_Foreign_C_Types_CShort))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Foreign.C.Types.CShort", ELam (v2 t_Foreign_C_Types_CShort) (create_integralCast_toInteger dc_CShort r_bits_short_ (EVar (v2 t_Foreign_C_Types_CShort))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Foreign.C.Types.CShort", ELam (v2 t_Foreign_C_Types_CShort) (build_abs "bits<short>" dc_CShort (EVar (v2 t_Foreign_C_Types_CShort))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Foreign.C.Types.CShort", ELam (v2 t_Foreign_C_Types_CShort) (build_signum "bits<short>" dc_CShort (EVar (v2 t_Foreign_C_Types_CShort)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Foreign.C.Types.CShort", op_aaB  Op.Eq "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Foreign.C.Types.CShort", op_aaB  Op.Gte "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Foreign.C.Types.CShort", op_aaB  Op.Lte "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Foreign.C.Types.CShort", op_aaB  Op.Gt "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Foreign.C.Types.CShort", op_aaB  Op.Lt "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Foreign.C.Types.CShort", op_aaa  Op.Add "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Foreign.C.Types.CShort", op_aaa  Op.Sub "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Foreign.C.Types.CShort", op_aaa  Op.Mul "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Foreign.C.Types.CShort", op_aa  Op.Neg "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Foreign.C.Types.CShort", op_aaa  Op.And "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Foreign.C.Types.CShort", op_aaa  Op.Or "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Foreign.C.Types.CShort", op_aaa  Op.Xor "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Foreign.C.Types.CShort", op_aa  Op.Com "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Foreign.C.Types.CShort", op_aaa  Op.Div "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Foreign.C.Types.CShort", op_aaa  Op.Mod "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Foreign.C.Types.CShort", op_aaa  Op.Div "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Foreign.C.Types.CShort", op_aaa  Op.Mod "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Foreign.C.Types.CShort", op_aIa  Op.Shl "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Foreign.C.Types.CShort", op_aIa  Op.Shra "bits<short>" dc_CShort t_Foreign_C_Types_CShort)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Foreign.C.Types.CInt", ELam (v0 t_Foreign_C_Types_CInt) $ prim_sizeof "bits<int>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Foreign.C.Types.CInt", buildPoke dc_CInt t_Foreign_C_Types_CInt "bits<int>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Foreign.C.Types.CInt", buildPeek dc_CInt t_Foreign_C_Types_CInt "bits<int>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Foreign.C.Types.CInt", prim_maxbound dc_CInt t_Foreign_C_Types_CInt "bits<int>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Foreign.C.Types.CInt", prim_minbound dc_CInt t_Foreign_C_Types_CInt "bits<int>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Foreign.C.Types.CInt", ELam v2_Int (create_integralCast_fromInt dc_CInt r_bits_int_ (EVar v2_Int) t_Foreign_C_Types_CInt))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Foreign.C.Types.CInt", ELam (v2 t_Foreign_C_Types_CInt) (create_integralCast_toInt dc_CInt r_bits_int_ (EVar (v2 t_Foreign_C_Types_CInt))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Foreign.C.Types.CInt", ELam v2_Integer (create_integralCast_fromInteger dc_CInt r_bits_int_ (EVar v2_Integer) t_Foreign_C_Types_CInt))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Foreign.C.Types.CInt", ELam (v2 t_Foreign_C_Types_CInt) (create_integralCast_toInteger dc_CInt r_bits_int_ (EVar (v2 t_Foreign_C_Types_CInt))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Foreign.C.Types.CInt", ELam (v2 t_Foreign_C_Types_CInt) (build_abs "bits<int>" dc_CInt (EVar (v2 t_Foreign_C_Types_CInt))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Foreign.C.Types.CInt", ELam (v2 t_Foreign_C_Types_CInt) (build_signum "bits<int>" dc_CInt (EVar (v2 t_Foreign_C_Types_CInt)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Foreign.C.Types.CInt", op_aaB  Op.Eq "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Foreign.C.Types.CInt", op_aaB  Op.Gte "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Foreign.C.Types.CInt", op_aaB  Op.Lte "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Foreign.C.Types.CInt", op_aaB  Op.Gt "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Foreign.C.Types.CInt", op_aaB  Op.Lt "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Foreign.C.Types.CInt", op_aaa  Op.Add "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Foreign.C.Types.CInt", op_aaa  Op.Sub "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Foreign.C.Types.CInt", op_aaa  Op.Mul "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Foreign.C.Types.CInt", op_aa  Op.Neg "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Foreign.C.Types.CInt", op_aaa  Op.And "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Foreign.C.Types.CInt", op_aaa  Op.Or "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Foreign.C.Types.CInt", op_aaa  Op.Xor "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Foreign.C.Types.CInt", op_aa  Op.Com "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Foreign.C.Types.CInt", op_aaa  Op.Div "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Foreign.C.Types.CInt", op_aaa  Op.Mod "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Foreign.C.Types.CInt", op_aaa  Op.Div "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Foreign.C.Types.CInt", op_aaa  Op.Mod "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Foreign.C.Types.CInt", op_aIa  Op.Shl "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Foreign.C.Types.CInt", op_aIa  Op.Shra "bits<int>" dc_CInt t_Foreign_C_Types_CInt)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Foreign.C.Types.CUInt", ELam (v0 t_Foreign_C_Types_CUInt) $ prim_sizeof "bits<int>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Foreign.C.Types.CUInt", buildPoke dc_CUInt t_Foreign_C_Types_CUInt "bits<int>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Foreign.C.Types.CUInt", buildPeek dc_CUInt t_Foreign_C_Types_CUInt "bits<int>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Foreign.C.Types.CUInt", prim_umaxbound dc_CUInt t_Foreign_C_Types_CUInt "bits<int>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Foreign.C.Types.CUInt", prim_uminbound dc_CUInt t_Foreign_C_Types_CUInt "bits<int>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Foreign.C.Types.CUInt", ELam v2_Int (create_uintegralCast_fromInt dc_CUInt r_bits_int_ (EVar v2_Int) t_Foreign_C_Types_CUInt))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Foreign.C.Types.CUInt", ELam (v2 t_Foreign_C_Types_CUInt) (create_uintegralCast_toInt dc_CUInt r_bits_int_ (EVar (v2 t_Foreign_C_Types_CUInt))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Foreign.C.Types.CUInt", ELam v2_Integer (create_uintegralCast_fromInteger dc_CUInt r_bits_int_ (EVar v2_Integer) t_Foreign_C_Types_CUInt))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Foreign.C.Types.CUInt", ELam (v2 t_Foreign_C_Types_CUInt) (create_uintegralCast_toInteger dc_CUInt r_bits_int_ (EVar (v2 t_Foreign_C_Types_CUInt))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Foreign.C.Types.CUInt", ELam (v2 t_Foreign_C_Types_CUInt) (build_uabs "bits<int>" dc_CUInt (EVar (v2 t_Foreign_C_Types_CUInt))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Foreign.C.Types.CUInt", ELam (v2 t_Foreign_C_Types_CUInt) (build_usignum "bits<int>" dc_CUInt (EVar (v2 t_Foreign_C_Types_CUInt)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Foreign.C.Types.CUInt", op_aaB  Op.Eq "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Foreign.C.Types.CUInt", op_aaB  Op.UGte "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Foreign.C.Types.CUInt", op_aaB  Op.ULte "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Foreign.C.Types.CUInt", op_aaB  Op.UGt "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Foreign.C.Types.CUInt", op_aaB  Op.ULt "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Foreign.C.Types.CUInt", op_aaa  Op.Add "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Foreign.C.Types.CUInt", op_aaa  Op.Sub "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Foreign.C.Types.CUInt", op_aaa  Op.Mul "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Foreign.C.Types.CUInt", op_aa  Op.Neg "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Foreign.C.Types.CUInt", op_aaa  Op.And "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Foreign.C.Types.CUInt", op_aaa  Op.Or "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Foreign.C.Types.CUInt", op_aaa  Op.Xor "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Foreign.C.Types.CUInt", op_aa  Op.Com "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Foreign.C.Types.CUInt", op_aaa  Op.UDiv "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Foreign.C.Types.CUInt", op_aaa  Op.UMod "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Foreign.C.Types.CUInt", op_aaa  Op.UDiv "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Foreign.C.Types.CUInt", op_aaa  Op.UMod "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Foreign.C.Types.CUInt", op_aIa  Op.Shl "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Foreign.C.Types.CUInt", op_aIa  Op.Shr "bits<int>" dc_CUInt t_Foreign_C_Types_CUInt)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Foreign.C.Types.CSize", ELam (v0 t_Foreign_C_Types_CSize) $ prim_sizeof "bits<size_t>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Foreign.C.Types.CSize", buildPoke dc_CSize t_Foreign_C_Types_CSize "bits<size_t>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Foreign.C.Types.CSize", buildPeek dc_CSize t_Foreign_C_Types_CSize "bits<size_t>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Foreign.C.Types.CSize", prim_umaxbound dc_CSize t_Foreign_C_Types_CSize "bits<size_t>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Foreign.C.Types.CSize", prim_uminbound dc_CSize t_Foreign_C_Types_CSize "bits<size_t>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Foreign.C.Types.CSize", ELam v2_Int (create_uintegralCast_fromInt dc_CSize r_bits_size_t_ (EVar v2_Int) t_Foreign_C_Types_CSize))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Foreign.C.Types.CSize", ELam (v2 t_Foreign_C_Types_CSize) (create_uintegralCast_toInt dc_CSize r_bits_size_t_ (EVar (v2 t_Foreign_C_Types_CSize))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Foreign.C.Types.CSize", ELam v2_Integer (create_uintegralCast_fromInteger dc_CSize r_bits_size_t_ (EVar v2_Integer) t_Foreign_C_Types_CSize))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Foreign.C.Types.CSize", ELam (v2 t_Foreign_C_Types_CSize) (create_uintegralCast_toInteger dc_CSize r_bits_size_t_ (EVar (v2 t_Foreign_C_Types_CSize))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Foreign.C.Types.CSize", ELam (v2 t_Foreign_C_Types_CSize) (build_uabs "bits<size_t>" dc_CSize (EVar (v2 t_Foreign_C_Types_CSize))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Foreign.C.Types.CSize", ELam (v2 t_Foreign_C_Types_CSize) (build_usignum "bits<size_t>" dc_CSize (EVar (v2 t_Foreign_C_Types_CSize)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Foreign.C.Types.CSize", op_aaB  Op.Eq "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Foreign.C.Types.CSize", op_aaB  Op.UGte "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Foreign.C.Types.CSize", op_aaB  Op.ULte "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Foreign.C.Types.CSize", op_aaB  Op.UGt "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Foreign.C.Types.CSize", op_aaB  Op.ULt "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Foreign.C.Types.CSize", op_aaa  Op.Add "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Foreign.C.Types.CSize", op_aaa  Op.Sub "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Foreign.C.Types.CSize", op_aaa  Op.Mul "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Foreign.C.Types.CSize", op_aa  Op.Neg "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Foreign.C.Types.CSize", op_aaa  Op.And "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Foreign.C.Types.CSize", op_aaa  Op.Or "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Foreign.C.Types.CSize", op_aaa  Op.Xor "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Foreign.C.Types.CSize", op_aa  Op.Com "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Foreign.C.Types.CSize", op_aaa  Op.UDiv "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Foreign.C.Types.CSize", op_aaa  Op.UMod "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Foreign.C.Types.CSize", op_aaa  Op.UDiv "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Foreign.C.Types.CSize", op_aaa  Op.UMod "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Foreign.C.Types.CSize", op_aIa  Op.Shl "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Foreign.C.Types.CSize", op_aIa  Op.Shr "bits<size_t>" dc_CSize t_Foreign_C_Types_CSize)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Foreign.C.Types.CWchar", ELam (v0 t_Foreign_C_Types_CWchar) $ prim_sizeof "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Foreign.C.Types.CWchar", buildPoke dc_CWchar t_Foreign_C_Types_CWchar "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Foreign.C.Types.CWchar", buildPeek dc_CWchar t_Foreign_C_Types_CWchar "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Foreign.C.Types.CWchar", prim_umaxbound dc_CWchar t_Foreign_C_Types_CWchar "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Foreign.C.Types.CWchar", prim_uminbound dc_CWchar t_Foreign_C_Types_CWchar "bits32")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Foreign.C.Types.CWchar", ELam v2_Int (create_uintegralCast_fromInt dc_CWchar r_bits32 (EVar v2_Int) t_Foreign_C_Types_CWchar))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Foreign.C.Types.CWchar", ELam (v2 t_Foreign_C_Types_CWchar) (create_uintegralCast_toInt dc_CWchar r_bits32 (EVar (v2 t_Foreign_C_Types_CWchar))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Foreign.C.Types.CWchar", ELam v2_Integer (create_uintegralCast_fromInteger dc_CWchar r_bits32 (EVar v2_Integer) t_Foreign_C_Types_CWchar))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Foreign.C.Types.CWchar", ELam (v2 t_Foreign_C_Types_CWchar) (create_uintegralCast_toInteger dc_CWchar r_bits32 (EVar (v2 t_Foreign_C_Types_CWchar))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Foreign.C.Types.CWchar", ELam (v2 t_Foreign_C_Types_CWchar) (build_uabs "bits32" dc_CWchar (EVar (v2 t_Foreign_C_Types_CWchar))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Foreign.C.Types.CWchar", ELam (v2 t_Foreign_C_Types_CWchar) (build_usignum "bits32" dc_CWchar (EVar (v2 t_Foreign_C_Types_CWchar)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Foreign.C.Types.CWchar", op_aaB  Op.Eq "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Foreign.C.Types.CWchar", op_aaB  Op.UGte "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Foreign.C.Types.CWchar", op_aaB  Op.ULte "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Foreign.C.Types.CWchar", op_aaB  Op.UGt "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Foreign.C.Types.CWchar", op_aaB  Op.ULt "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Foreign.C.Types.CWchar", op_aaa  Op.Add "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Foreign.C.Types.CWchar", op_aaa  Op.Sub "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Foreign.C.Types.CWchar", op_aaa  Op.Mul "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Foreign.C.Types.CWchar", op_aa  Op.Neg "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Foreign.C.Types.CWchar", op_aaa  Op.And "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Foreign.C.Types.CWchar", op_aaa  Op.Or "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Foreign.C.Types.CWchar", op_aaa  Op.Xor "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Foreign.C.Types.CWchar", op_aa  Op.Com "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Foreign.C.Types.CWchar", op_aaa  Op.UDiv "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Foreign.C.Types.CWchar", op_aaa  Op.UMod "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Foreign.C.Types.CWchar", op_aaa  Op.UDiv "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Foreign.C.Types.CWchar", op_aaa  Op.UMod "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Foreign.C.Types.CWchar", op_aIa  Op.Shl "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Foreign.C.Types.CWchar", op_aIa  Op.Shr "bits32" dc_CWchar t_Foreign_C_Types_CWchar)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Foreign.C.Types.CWint", ELam (v0 t_Foreign_C_Types_CWint) $ prim_sizeof "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Foreign.C.Types.CWint", buildPoke dc_CWint t_Foreign_C_Types_CWint "bits32")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Foreign.C.Types.CWint", buildPeek dc_CWint t_Foreign_C_Types_CWint "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Foreign.C.Types.CWint", prim_maxbound dc_CWint t_Foreign_C_Types_CWint "bits32")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Foreign.C.Types.CWint", prim_minbound dc_CWint t_Foreign_C_Types_CWint "bits32")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Foreign.C.Types.CWint", ELam v2_Int (create_integralCast_fromInt dc_CWint r_bits32 (EVar v2_Int) t_Foreign_C_Types_CWint))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInt.Foreign.C.Types.CWint", ELam (v2 t_Foreign_C_Types_CWint) (create_integralCast_toInt dc_CWint r_bits32 (EVar (v2 t_Foreign_C_Types_CWint))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Foreign.C.Types.CWint", ELam v2_Integer (create_integralCast_fromInteger dc_CWint r_bits32 (EVar v2_Integer) t_Foreign_C_Types_CWint))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.toInteger.Foreign.C.Types.CWint", ELam (v2 t_Foreign_C_Types_CWint) (create_integralCast_toInteger dc_CWint r_bits32 (EVar (v2 t_Foreign_C_Types_CWint))))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Foreign.C.Types.CWint", ELam (v2 t_Foreign_C_Types_CWint) (build_abs "bits32" dc_CWint (EVar (v2 t_Foreign_C_Types_CWint))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Foreign.C.Types.CWint", ELam (v2 t_Foreign_C_Types_CWint) (build_signum "bits32" dc_CWint (EVar (v2 t_Foreign_C_Types_CWint)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Foreign.C.Types.CWint", op_aaB  Op.Eq "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Foreign.C.Types.CWint", op_aaB  Op.Gte "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Foreign.C.Types.CWint", op_aaB  Op.Lte "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Foreign.C.Types.CWint", op_aaB  Op.Gt "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Foreign.C.Types.CWint", op_aaB  Op.Lt "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Foreign.C.Types.CWint", op_aaa  Op.Add "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Foreign.C.Types.CWint", op_aaa  Op.Sub "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Foreign.C.Types.CWint", op_aaa  Op.Mul "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Foreign.C.Types.CWint", op_aa  Op.Neg "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..&..Foreign.C.Types.CWint", op_aaa  Op.And "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits..|..Foreign.C.Types.CWint", op_aaa  Op.Or "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.xor.Foreign.C.Types.CWint", op_aaa  Op.Xor "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.complement.Foreign.C.Types.CWint", op_aa  Op.Com "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.quot.Foreign.C.Types.CWint", op_aaa  Op.Div "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.rem.Foreign.C.Types.CWint", op_aaa  Op.Mod "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.div.Foreign.C.Types.CWint", op_aaa  Op.Div "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Lhc_Num_Integral,toInstName "Lhc.Num.mod.Foreign.C.Types.CWint", op_aaa  Op.Mod "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftL.Foreign.C.Types.CWint", op_aIa  Op.Shl "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Data_Bits_Bits,toInstName "Data.Bits.shiftR.Foreign.C.Types.CWint", op_aIa  Op.Shra "bits32" dc_CWint t_Foreign_C_Types_CWint)
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.sizeOf.Foreign.C.Types.CTime", ELam (v0 t_Foreign_C_Types_CTime) $ prim_sizeof "bits<time_t>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.poke.Foreign.C.Types.CTime", buildPoke dc_CTime t_Foreign_C_Types_CTime "bits<time_t>")
  ,(n_Foreign_Storable_Storable, toInstName "Foreign.Storable.peek.Foreign.C.Types.CTime", buildPeek dc_CTime t_Foreign_C_Types_CTime "bits<time_t>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.maxBound.Foreign.C.Types.CTime", prim_umaxbound dc_CTime t_Foreign_C_Types_CTime "bits<time_t>")
  ,(n_Lhc_Enum_Bounded, toInstName "Lhc.Enum.minBound.Foreign.C.Types.CTime", prim_uminbound dc_CTime t_Foreign_C_Types_CTime "bits<time_t>")
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInt.Foreign.C.Types.CTime", ELam v2_Int (create_uintegralCast_fromInt dc_CTime r_bits_time_t_ (EVar v2_Int) t_Foreign_C_Types_CTime))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.fromInteger.Foreign.C.Types.CTime", ELam v2_Integer (create_uintegralCast_fromInteger dc_CTime r_bits_time_t_ (EVar v2_Integer) t_Foreign_C_Types_CTime))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.abs.Foreign.C.Types.CTime", ELam (v2 t_Foreign_C_Types_CTime) (build_uabs "bits<time_t>" dc_CTime (EVar (v2 t_Foreign_C_Types_CTime))  ))
  ,(n_Lhc_Num_Num, toInstName "Lhc.Num.signum.Foreign.C.Types.CTime", ELam (v2 t_Foreign_C_Types_CTime) (build_usignum "bits<time_t>" dc_CTime (EVar (v2 t_Foreign_C_Types_CTime)) ))
  ,(n_Lhc_Order_Eq,toInstName "Lhc.Order.==.Foreign.C.Types.CTime", op_aaB  Op.Eq "bits<time_t>" dc_CTime t_Foreign_C_Types_CTime)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>=.Foreign.C.Types.CTime", op_aaB  Op.UGte "bits<time_t>" dc_CTime t_Foreign_C_Types_CTime)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<=.Foreign.C.Types.CTime", op_aaB  Op.ULte "bits<time_t>" dc_CTime t_Foreign_C_Types_CTime)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.>.Foreign.C.Types.CTime", op_aaB  Op.UGt "bits<time_t>" dc_CTime t_Foreign_C_Types_CTime)
  ,(n_Lhc_Order_Ord,toInstName "Lhc.Order.<.Foreign.C.Types.CTime", op_aaB  Op.ULt "bits<time_t>" dc_CTime t_Foreign_C_Types_CTime)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.+.Foreign.C.Types.CTime", op_aaa  Op.Add "bits<time_t>" dc_CTime t_Foreign_C_Types_CTime)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.-.Foreign.C.Types.CTime", op_aaa  Op.Sub "bits<time_t>" dc_CTime t_Foreign_C_Types_CTime)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.*.Foreign.C.Types.CTime", op_aaa  Op.Mul "bits<time_t>" dc_CTime t_Foreign_C_Types_CTime)
  ,(n_Lhc_Num_Num,toInstName "Lhc.Num.negate.Foreign.C.Types.CTime", op_aa  Op.Neg "bits<time_t>" dc_CTime t_Foreign_C_Types_CTime) ]

theMethods = [
    ]

allCTypes = [
   (dc_Integer, tc_Integer, r_bits_max_, "bits<max>", "int")
  ,(dc_Int8, tc_Int8, r_bits8, "bits8", "int")
  ,(dc_Int16, tc_Int16, r_bits16, "bits16", "int")
  ,(dc_Int32, tc_Int32, r_bits32, "bits32", "int")
  ,(dc_Int64, tc_Int64, r_bits64, "bits64", "int")
  ,(dc_IntMax, tc_IntMax, r_bits_max_, "bits<max>", "int")
  ,(dc_IntPtr, tc_IntPtr, r_bits_ptr_, "bits<ptr>", "int")
  ,(dc_Word, tc_Word, r_bits32, "bits32", "int")
  ,(dc_Word8, tc_Word8, r_bits8, "bits8", "int")
  ,(dc_Word16, tc_Word16, r_bits16, "bits16", "int")
  ,(dc_Word32, tc_Word32, r_bits32, "bits32", "int")
  ,(dc_Word64, tc_Word64, r_bits64, "bits64", "int")
  ,(dc_WordMax, tc_WordMax, r_bits_max_, "bits<max>", "int")
  ,(dc_WordPtr, tc_WordPtr, r_bits_ptr_, "bits<ptr>", "int")
  ,(dc_CChar, tc_CChar, r_bits8, "bits8", "int")
  ,(dc_CShort, tc_CShort, r_bits_short_, "bits<short>", "int")
  ,(dc_CInt, tc_CInt, r_bits_int_, "bits<int>", "int")
  ,(dc_CUInt, tc_CUInt, r_bits_int_, "bits<int>", "int")
  ,(dc_CSize, tc_CSize, r_bits_size_t_, "bits<size_t>", "int")
  ,(dc_CWchar, tc_CWchar, r_bits32, "bits32", "int")
  ,(dc_CWint, tc_CWint, r_bits32, "bits32", "int")
  ,(dc_CTime, tc_CTime, r_bits_time_t_, "bits<time_t>", "float")
 ]

t_Data_Word_WordMax = ELit litCons { litName = tc_WordMax, litType = eStar}
t_Lhc_Basics_Integer = ELit litCons { litName = tc_Integer, litType = eStar}
t_Foreign_C_Types_CInt = ELit litCons { litName = tc_CInt, litType = eStar}
t_Data_Word_Word64 = ELit litCons { litName = tc_Word64, litType = eStar}
t_Data_Int_IntMax = ELit litCons { litName = tc_IntMax, litType = eStar}
t_Foreign_C_Types_CTime = ELit litCons { litName = tc_CTime, litType = eStar}
t_Foreign_C_Types_CShort = ELit litCons { litName = tc_CShort, litType = eStar}
t_Data_Int_Int8 = ELit litCons { litName = tc_Int8, litType = eStar}
t_Foreign_C_Types_CChar = ELit litCons { litName = tc_CChar, litType = eStar}
t_Data_Word_Word = ELit litCons { litName = tc_Word, litType = eStar}
t_Data_Int_Int16 = ELit litCons { litName = tc_Int16, litType = eStar}
t_Data_Word_Word8 = ELit litCons { litName = tc_Word8, litType = eStar}
t_Data_Word_Word32 = ELit litCons { litName = tc_Word32, litType = eStar}
t_Foreign_C_Types_CWchar = ELit litCons { litName = tc_CWchar, litType = eStar}
t_Foreign_C_Types_CSize = ELit litCons { litName = tc_CSize, litType = eStar}
t_Data_Word_Word16 = ELit litCons { litName = tc_Word16, litType = eStar}
t_Foreign_C_Types_CUInt = ELit litCons { litName = tc_CUInt, litType = eStar}
t_Data_Int_Int32 = ELit litCons { litName = tc_Int32, litType = eStar}
t_Foreign_C_Types_CWint = ELit litCons { litName = tc_CWint, litType = eStar}
t_Data_Int_Int64 = ELit litCons { litName = tc_Int64, litType = eStar}
t_Data_Int_IntPtr = ELit litCons { litName = tc_IntPtr, litType = eStar}
t_Data_Word_WordPtr = ELit litCons { litName = tc_WordPtr, litType = eStar}

tc_Data_Word_WordMax = TCon (Tycon tc_WordMax kindStar)
tc_Lhc_Basics_Integer = TCon (Tycon tc_Integer kindStar)
tc_Foreign_C_Types_CInt = TCon (Tycon tc_CInt kindStar)
tc_Data_Word_Word64 = TCon (Tycon tc_Word64 kindStar)
tc_Data_Int_IntMax = TCon (Tycon tc_IntMax kindStar)
tc_Foreign_C_Types_CTime = TCon (Tycon tc_CTime kindStar)
tc_Foreign_C_Types_CShort = TCon (Tycon tc_CShort kindStar)
tc_Data_Int_Int8 = TCon (Tycon tc_Int8 kindStar)
tc_Foreign_C_Types_CChar = TCon (Tycon tc_CChar kindStar)
tc_Data_Word_Word = TCon (Tycon tc_Word kindStar)
tc_Data_Int_Int16 = TCon (Tycon tc_Int16 kindStar)
tc_Data_Word_Word8 = TCon (Tycon tc_Word8 kindStar)
tc_Data_Word_Word32 = TCon (Tycon tc_Word32 kindStar)
tc_Foreign_C_Types_CWchar = TCon (Tycon tc_CWchar kindStar)
tc_Foreign_C_Types_CSize = TCon (Tycon tc_CSize kindStar)
tc_Data_Word_Word16 = TCon (Tycon tc_Word16 kindStar)
tc_Foreign_C_Types_CUInt = TCon (Tycon tc_CUInt kindStar)
tc_Data_Int_Int32 = TCon (Tycon tc_Int32 kindStar)
tc_Foreign_C_Types_CWint = TCon (Tycon tc_CWint kindStar)
tc_Data_Int_Int64 = TCon (Tycon tc_Int64 kindStar)
tc_Data_Int_IntPtr = TCon (Tycon tc_IntPtr kindStar)
tc_Data_Word_WordPtr = TCon (Tycon tc_WordPtr kindStar)

n_Lhc_Num_Integral = toClassName "Lhc.Num.Integral"
n_Lhc_Num_Num = toClassName "Lhc.Num.Num"
n_Data_Bits_Bits = toClassName "Data.Bits.Bits"
n_Lhc_Order_Eq = toClassName "Lhc.Order.Eq"
n_Lhc_Enum_Bounded = toClassName "Lhc.Enum.Bounded"
n_Lhc_Order_Ord = toClassName "Lhc.Order.Ord"
n_Foreign_Storable_Storable = toClassName "Foreign.Storable.Storable"

{-# NOINLINE n_Lhc_Num_Integral #-}
{-# NOINLINE n_Lhc_Num_Num #-}
{-# NOINLINE n_Data_Bits_Bits #-}
{-# NOINLINE n_Lhc_Order_Eq #-}
{-# NOINLINE n_Lhc_Enum_Bounded #-}
{-# NOINLINE n_Lhc_Order_Ord #-}
{-# NOINLINE n_Foreign_Storable_Storable #-}



r_bits_max_    = ELit litCons { litName = rt_bits_max_, litType = eHash }
r_bits_ptr_    = ELit litCons { litName = rt_bits_ptr_, litType = eHash }
r_bits_int_    = ELit litCons { litName = rt_bits_int_, litType = eHash }
r_bits32       = ELit litCons { litName = rt_bits32, litType = eHash }
r_bits_short_  = ELit litCons { litName = rt_bits_short_, litType = eHash }
r_bits_size_t_ = ELit litCons { litName = rt_bits_size_t_, litType = eHash }
r_bits_time_t_ = ELit litCons { litName = rt_bits_time_t_, litType = eHash }
r_bits8        = ELit litCons { litName = rt_bits8, litType = eHash }
r_bits16       = ELit litCons { litName = rt_bits16, litType = eHash }
r_bits64       = ELit litCons { litName = rt_bits64, litType = eHash }



