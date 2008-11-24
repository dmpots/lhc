module Name.Prim where

import Name.Name

{-# NOINLINE tc_Int #-}
tc_Int = toName TypeConstructor ("Lhc.Prim","Int")
{-# NOINLINE dc_Int #-}
dc_Int = toName DataConstructor "Int#"
{-# NOINLINE tc_Integer #-}
tc_Integer = toName TypeConstructor ("Lhc.Basics","Integer")
{-# NOINLINE dc_Integer #-}
dc_Integer = toName DataConstructor "Integer#"
{-# NOINLINE tc_Int8 #-}
tc_Int8 = toName TypeConstructor ("Data.Int","Int8")
{-# NOINLINE dc_Int8 #-}
dc_Int8 = toName DataConstructor "Int8#"
{-# NOINLINE tc_Int16 #-}
tc_Int16 = toName TypeConstructor ("Data.Int","Int16")
{-# NOINLINE dc_Int16 #-}
dc_Int16 = toName DataConstructor "Int16#"
{-# NOINLINE tc_Int32 #-}
tc_Int32 = toName TypeConstructor ("Data.Int","Int32")
{-# NOINLINE dc_Int32 #-}
dc_Int32 = toName DataConstructor "Int32#"
{-# NOINLINE tc_Int64 #-}
tc_Int64 = toName TypeConstructor ("Data.Int","Int64")
{-# NOINLINE dc_Int64 #-}
dc_Int64 = toName DataConstructor "Int64#"
{-# NOINLINE tc_IntMax #-}
tc_IntMax = toName TypeConstructor ("Data.Int","IntMax")
{-# NOINLINE dc_IntMax #-}
dc_IntMax = toName DataConstructor "IntMax#"
{-# NOINLINE tc_IntPtr #-}
tc_IntPtr = toName TypeConstructor ("Data.Int","IntPtr")
{-# NOINLINE dc_IntPtr #-}
dc_IntPtr = toName DataConstructor "IntPtr#"
{-# NOINLINE tc_Word #-}
tc_Word = toName TypeConstructor ("Data.Word","Word")
{-# NOINLINE dc_Word #-}
dc_Word = toName DataConstructor "Word#"
{-# NOINLINE tc_Word8 #-}
tc_Word8 = toName TypeConstructor ("Data.Word","Word8")
{-# NOINLINE dc_Word8 #-}
dc_Word8 = toName DataConstructor "Word8#"
{-# NOINLINE tc_Word16 #-}
tc_Word16 = toName TypeConstructor ("Data.Word","Word16")
{-# NOINLINE dc_Word16 #-}
dc_Word16 = toName DataConstructor "Word16#"
{-# NOINLINE tc_Word32 #-}
tc_Word32 = toName TypeConstructor ("Data.Word","Word32")
{-# NOINLINE dc_Word32 #-}
dc_Word32 = toName DataConstructor "Word32#"
{-# NOINLINE tc_Word64 #-}
tc_Word64 = toName TypeConstructor ("Data.Word","Word64")
{-# NOINLINE dc_Word64 #-}
dc_Word64 = toName DataConstructor "Word64#"
{-# NOINLINE tc_WordMax #-}
tc_WordMax = toName TypeConstructor ("Data.Word","WordMax")
{-# NOINLINE dc_WordMax #-}
dc_WordMax = toName DataConstructor "WordMax#"
{-# NOINLINE tc_WordPtr #-}
tc_WordPtr = toName TypeConstructor ("Data.Word","WordPtr")
{-# NOINLINE dc_WordPtr #-}
dc_WordPtr = toName DataConstructor "WordPtr#"
{-# NOINLINE tc_CChar #-}
tc_CChar = toName TypeConstructor ("Foreign.C.Types","CChar")
{-# NOINLINE dc_CChar #-}
dc_CChar = toName DataConstructor "CChar#"
{-# NOINLINE tc_CShort #-}
tc_CShort = toName TypeConstructor ("Foreign.C.Types","CShort")
{-# NOINLINE dc_CShort #-}
dc_CShort = toName DataConstructor "CShort#"
{-# NOINLINE tc_CInt #-}
tc_CInt = toName TypeConstructor ("Foreign.C.Types","CInt")
{-# NOINLINE dc_CInt #-}
dc_CInt = toName DataConstructor "CInt#"
{-# NOINLINE tc_CUInt #-}
tc_CUInt = toName TypeConstructor ("Foreign.C.Types","CUInt")
{-# NOINLINE dc_CUInt #-}
dc_CUInt = toName DataConstructor "CUInt#"
{-# NOINLINE tc_CSize #-}
tc_CSize = toName TypeConstructor ("Foreign.C.Types","CSize")
{-# NOINLINE dc_CSize #-}
dc_CSize = toName DataConstructor "CSize#"
{-# NOINLINE tc_CWchar #-}
tc_CWchar = toName TypeConstructor ("Foreign.C.Types","CWchar")
{-# NOINLINE dc_CWchar #-}
dc_CWchar = toName DataConstructor "CWchar#"
{-# NOINLINE tc_CWint #-}
tc_CWint = toName TypeConstructor ("Foreign.C.Types","CWint")
{-# NOINLINE dc_CWint #-}
dc_CWint = toName DataConstructor "CWint#"
{-# NOINLINE tc_CTime #-}
tc_CTime = toName TypeConstructor ("Foreign.C.Types","CTime")
{-# NOINLINE dc_CTime #-}
dc_CTime = toName DataConstructor "CTime#"

{-# NOINLINE rt_bits16 #-}
rt_bits16 = toName RawType "bits16"
{-# NOINLINE rt_bits32 #-}
rt_bits32 = toName RawType "bits32"
{-# NOINLINE rt_bits64 #-}
rt_bits64 = toName RawType "bits64"
{-# NOINLINE rt_bits8 #-}
rt_bits8 = toName RawType "bits8"
{-# NOINLINE rt_bits_int_ #-}
rt_bits_int_ = toName RawType "bits<int>"
{-# NOINLINE rt_bits_max_ #-}
rt_bits_max_ = toName RawType "bits<max>"
{-# NOINLINE rt_bits_ptr_ #-}
rt_bits_ptr_ = toName RawType "bits<ptr>"
{-# NOINLINE rt_bits_short_ #-}
rt_bits_short_ = toName RawType "bits<short>"
{-# NOINLINE rt_bits_size_t_ #-}
rt_bits_size_t_ = toName RawType "bits<size_t>"
{-# NOINLINE rt_bits_time_t_ #-}
rt_bits_time_t_ = toName RawType "bits<time_t>"
{-# NOINLINE tc_JumpPoint #-}
tc_JumpPoint = toName TypeConstructor ("Lhc.JumpPoint","JumpPoint")
{-# NOINLINE tc_Char #-}
tc_Char = toName TypeConstructor ("Lhc.Prim","Char")
{-# NOINLINE tc_IO #-}
tc_IO = toName TypeConstructor ("Lhc.Prim","IO")
{-# NOINLINE tc_World__ #-}
tc_World__ = toName TypeConstructor ("Lhc.Prim","World__")
{-# NOINLINE tc_Bool #-}
tc_Bool = toName TypeConstructor ("Lhc.Order","Bool")
{-# NOINLINE tc_Target #-}
tc_Target = toName TypeConstructor ("Lhc.Options","Target")
{-# NOINLINE tc_Ptr #-}
tc_Ptr = toName TypeConstructor ("Lhc.Addr","Ptr")
{-# NOINLINE tc_Addr #-}
tc_Addr = toName TypeConstructor ("Lhc.Addr","Addr")
{-# NOINLINE tc_FunAddr #-}
tc_FunAddr = toName TypeConstructor ("Lhc.Addr","FunAddr")
{-# NOINLINE tc_Ratio #-}
tc_Ratio = toName TypeConstructor ("Lhc.Num","Ratio")
{-# NOINLINE tc_Unit #-}
tc_Unit = toName TypeConstructor ("Lhc.Basics","()")
{-# NOINLINE tc_Float #-}
tc_Float = toName TypeConstructor ("Lhc.Float","Float")
{-# NOINLINE tc_Double #-}
tc_Double = toName TypeConstructor ("Lhc.Float","Double")
{-# NOINLINE tc_CLong #-}
tc_CLong = toName TypeConstructor ("Foreign.C.Types","CLong")
{-# NOINLINE tc_CSChar #-}
tc_CSChar = toName TypeConstructor ("Foreign.C.Types","CSChar")
{-# NOINLINE tc_CUChar #-}
tc_CUChar = toName TypeConstructor ("Foreign.C.Types","CUChar")
{-# NOINLINE tc_CUShort #-}
tc_CUShort = toName TypeConstructor ("Foreign.C.Types","CUShort")
{-# NOINLINE tc_CULong #-}
tc_CULong = toName TypeConstructor ("Foreign.C.Types","CULong")
{-# NOINLINE tc_Bits1 #-}
tc_Bits1 = toName TypeConstructor ("Lhc.Types","Bits1_")
{-# NOINLINE tc_Bits8 #-}
tc_Bits8 = toName TypeConstructor ("Lhc.Types","Bits8_")
{-# NOINLINE tc_Bits16 #-}
tc_Bits16 = toName TypeConstructor ("Lhc.Types","Bits16_")
{-# NOINLINE tc_Bits32 #-}
tc_Bits32 = toName TypeConstructor ("Lhc.Types","Bits32_")
{-# NOINLINE tc_Bits64 #-}
tc_Bits64 = toName TypeConstructor ("Lhc.Types","Bits64_")
{-# NOINLINE tc_Bits128 #-}
tc_Bits128 = toName TypeConstructor ("Lhc.Types","Bits128_")
{-# NOINLINE tc_BitsPtr #-}
tc_BitsPtr = toName TypeConstructor ("Lhc.Types","BitsPtr_")
{-# NOINLINE tc_BitsMax #-}
tc_BitsMax = toName TypeConstructor ("Lhc.Types","BitsMax_")
{-# NOINLINE tc_Float32 #-}
tc_Float32 = toName TypeConstructor ("Lhc.Types","Float32_")
{-# NOINLINE tc_Float64 #-}
tc_Float64 = toName TypeConstructor ("Lhc.Types","Float64_")
{-# NOINLINE tc_Float80 #-}
tc_Float80 = toName TypeConstructor ("Lhc.Types","Float80_")
{-# NOINLINE tc_Float128 #-}
tc_Float128 = toName TypeConstructor ("Lhc.Types","Float128_")
{-# NOINLINE dc_Rational #-}
dc_Rational = toName DataConstructor ("Lhc.Num",":%")
{-# NOINLINE dc_Cons #-}
dc_Cons = toName DataConstructor ("Lhc.Prim",":")
{-# NOINLINE dc_EmptyList #-}
dc_EmptyList = toName DataConstructor ("Lhc.Prim","[]")
{-# NOINLINE dc_Unit #-}
dc_Unit = toName DataConstructor ("Lhc.Basics","()")
{-# NOINLINE dc_Boolzh #-}
dc_Boolzh = toName DataConstructor ("Lhc.Order","Bool#")
{-# NOINLINE dc_Target #-}
dc_Target = toName DataConstructor ("Lhc.Options","Target#")
{-# NOINLINE dc_Char #-}
dc_Char = toName DataConstructor ("Lhc.Prim","Char")
{-# NOINLINE dc_Addr #-}
dc_Addr = toName DataConstructor ("Lhc.Addr","Addr")
{-# NOINLINE class_Eq #-}
class_Eq = toName ClassName ("Lhc.Order","Eq")
{-# NOINLINE class_Ord #-}
class_Ord = toName ClassName ("Lhc.Order","Ord")
{-# NOINLINE class_Enum #-}
class_Enum = toName ClassName ("Lhc.Enum","Enum")
{-# NOINLINE class_Bounded #-}
class_Bounded = toName ClassName ("Lhc.Enum","Bounded")
{-# NOINLINE class_Show #-}
class_Show = toName ClassName ("Lhc.Show","Show")
{-# NOINLINE class_Read #-}
class_Read = toName ClassName ("Lhc.Text","Read")
{-# NOINLINE class_Ix #-}
class_Ix = toName ClassName ("Data.Ix","Ix")
{-# NOINLINE class_Functor #-}
class_Functor = toName ClassName ("Lhc.Monad","Functor")
{-# NOINLINE class_Monad #-}
class_Monad = toName ClassName ("Lhc.Monad","Monad")
{-# NOINLINE class_Num #-}
class_Num = toName ClassName ("Lhc.Num","Num")
{-# NOINLINE class_Real #-}
class_Real = toName ClassName ("Lhc.Num","Real")
{-# NOINLINE class_Integral #-}
class_Integral = toName ClassName ("Lhc.Num","Integral")
{-# NOINLINE class_Fractional #-}
class_Fractional = toName ClassName ("Lhc.Num","Fractional")
{-# NOINLINE class_Floating #-}
class_Floating = toName ClassName ("Lhc.Float","Floating")
{-# NOINLINE class_RealFrac #-}
class_RealFrac = toName ClassName ("Lhc.Float","RealFrac")
{-# NOINLINE class_RealFloat #-}
class_RealFloat = toName ClassName ("Lhc.Float","RealFloat")
{-# NOINLINE rt_bits128 #-}
rt_bits128 = toName RawType "bits128"
{-# NOINLINE rt_bool #-}
rt_bool = toName RawType "bool"
{-# NOINLINE rt_float32 #-}
rt_float32 = toName RawType "fbits32"
{-# NOINLINE rt_float64 #-}
rt_float64 = toName RawType "fbits64"
{-# NOINLINE rt_float80 #-}
rt_float80 = toName RawType "fbits80"
{-# NOINLINE rt_float128 #-}
rt_float128 = toName RawType "fbits128"
{-# NOINLINE v_eqString #-}
v_eqString = toName Val ("Lhc.String","eqString")
{-# NOINLINE v_eqUnpackedString #-}
v_eqUnpackedString = toName Val ("Lhc.String","eqUnpackedString")
{-# NOINLINE v_unpackString #-}
v_unpackString = toName Val ("Lhc.String","unpackString")
{-# NOINLINE v_target #-}
v_target = toName Val ("Lhc.Options","target")
{-# NOINLINE v_error #-}
v_error = toName Val ("Lhc.IO","error")
{-# NOINLINE v_minBound #-}
v_minBound = toName Val ("Lhc.Enum","minBound")
{-# NOINLINE v_maxBound #-}
v_maxBound = toName Val ("Lhc.Enum","maxBound")
{-# NOINLINE v_fail #-}
v_fail = toName Val ("Lhc.Monad","fail")
{-# NOINLINE v_map #-}
v_map = toName Val ("Lhc.Basics","map")
{-# NOINLINE v_and #-}
v_and = toName Val ("Lhc.Order","&&")
{-# NOINLINE v_filter #-}
v_filter = toName Val ("Lhc.List","filter")
{-# NOINLINE v_foldr #-}
v_foldr = toName Val ("Lhc.Basics","foldr")
{-# NOINLINE v_undefined #-}
v_undefined = toName Val ("Lhc.Basics","undefined")
{-# NOINLINE v_bind #-}
v_bind = toName Val ("Lhc.Monad",">>=")
{-# NOINLINE v_bind_ #-}
v_bind_ = toName Val ("Lhc.Monad",">>")
{-# NOINLINE v_return #-}
v_return = toName Val ("Lhc.Monad","return")
{-# NOINLINE v_concatMap #-}
v_concatMap = toName Val ("Lhc.Basics","concatMap")
{-# NOINLINE v_fromInteger #-}
v_fromInteger = toName Val ("Lhc.Num","fromInteger")
{-# NOINLINE v_fromInt #-}
v_fromInt = toName Val ("Lhc.Num","fromInt")
{-# NOINLINE v_fromRational #-}
v_fromRational = toName Val ("Lhc.Num","fromRational")
{-# NOINLINE v_negate #-}
v_negate = toName Val ("Lhc.Num","negate")
{-# NOINLINE v_leq #-}
v_leq = toName Val ("Lhc.Order","<=")
{-# NOINLINE v_geq #-}
v_geq = toName Val ("Lhc.Order",">=")
{-# NOINLINE v_lt #-}
v_lt = toName Val ("Lhc.Order","<")
{-# NOINLINE v_gt #-}
v_gt = toName Val ("Lhc.Order",">")
{-# NOINLINE v_compare #-}
v_compare = toName Val ("Lhc.Order","compare")
{-# NOINLINE v_equals #-}
v_equals = toName Val ("Lhc.Order","==")
{-# NOINLINE v_neq #-}
v_neq = toName Val ("Lhc.Order","/=")
{-# NOINLINE v_fromEnum #-}
v_fromEnum = toName Val ("Lhc.Enum","fromEnum")
{-# NOINLINE v_toEnum #-}
v_toEnum = toName Val ("Lhc.Enum","toEnum")
{-# NOINLINE v_enumFrom #-}
v_enumFrom = toName Val ("Lhc.Enum","enumFrom")
{-# NOINLINE v_enumFromTo #-}
v_enumFromTo = toName Val ("Lhc.Enum","enumFromTo")
{-# NOINLINE v_enumFromThenTo #-}
v_enumFromThenTo = toName Val ("Lhc.Enum","enumFromThenTo")
{-# NOINLINE v_enumFromThen #-}
v_enumFromThen = toName Val ("Lhc.Enum","enumFromThen")
{-# NOINLINE v_succ #-}
v_succ = toName Val ("Lhc.Enum","succ")
{-# NOINLINE v_pred #-}
v_pred = toName Val ("Lhc.Enum","pred")
{-# NOINLINE v_range #-}
v_range = toName Val ("Data.Ix","range")
{-# NOINLINE v_index #-}
v_index = toName Val ("Data.Ix","index")
{-# NOINLINE v_inRange #-}
v_inRange = toName Val ("Data.Ix","inRange")
{-# NOINLINE v_runExpr #-}
v_runExpr = toName Val ("Prelude.IO","runExpr")
{-# NOINLINE v_runRaw #-}
v_runRaw = toName Val ("Lhc.Prim","runRaw")
{-# NOINLINE v_runMain #-}
v_runMain = toName Val ("Lhc.IO","runMain")
{-# NOINLINE v_runNoWrapper #-}
v_runNoWrapper = toName Val ("Lhc.Prim","runNoWrapper")
{-# NOINLINE v_enum_succ #-}
v_enum_succ = toName Val ("Lhc.Inst.PrimEnum","enum_succ")
{-# NOINLINE v_enum_pred #-}
v_enum_pred = toName Val ("Lhc.Inst.PrimEnum","enum_pred")
{-# NOINLINE v_enum_from #-}
v_enum_from = toName Val ("Lhc.Inst.PrimEnum","enum_from")
{-# NOINLINE v_enum_fromTo #-}
v_enum_fromTo = toName Val ("Lhc.Inst.PrimEnum","enum_fromTo")
{-# NOINLINE v_enum_fromThen #-}
v_enum_fromThen = toName Val ("Lhc.Inst.PrimEnum","enum_fromThen")
{-# NOINLINE v_enum_fromThenTo #-}
v_enum_fromThenTo = toName Val ("Lhc.Inst.PrimEnum","enum_fromThenTo")
{-# NOINLINE v_enum_toEnum #-}
v_enum_toEnum = toName Val ("Lhc.Inst.PrimEnum","enum_toEnum")


