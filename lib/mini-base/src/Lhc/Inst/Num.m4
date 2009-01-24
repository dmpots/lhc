m4_define(NUMINST,{{
instance Num $1 where
    $1 x + $1 y = $1 (add_$1 x y)
    $1 x - $1 y = $1 (sub_$1 x y)
    $1 x * $1 y = $1 (mul_$1 x y)
    
    negate ($1 x) = $1 (neg_$1 x)
    
    abs    x | x < 0     = -x
             | otherwise =  x
    
    signum 0 = 0
    signum x | x < 0     = -1
             | otherwise =  1

    fromInteger (Integer x) = $1 (bitsmax_to_$1 x)
    fromInt (Int x) = $1 (bits32_to_$1 x)

instance Integral $1 where
    $1 n `quot` $1 d = $1 (div_$1 n d)
    $1 n `rem`  $1 d = $1 (mod_$1 n d)

    toInteger ($1 x) = Integer (max_from_$1 x)
    toInt ($1 x) = Int (bits32_from_$1 x)

foreign import primitive "Neg" neg_$1 :: $2 -> $2
foreign import primitive "Add" add_$1 :: $2 -> $2 -> $2
foreign import primitive "Sub" sub_$1 :: $2 -> $2 -> $2
foreign import primitive "Mul" mul_$1 :: $2 -> $2 -> $2
foreign import primitive "$3Div" div_$1 :: $2 -> $2 -> $2
foreign import primitive "$3Mod" mod_$1 :: $2 -> $2 -> $2

foreign import primitive "I2I" bitsmax_to_$1 :: BitsMax_ -> $2
foreign import primitive "I2I" bits32_to_$1 :: Bits32_ -> $2
foreign import primitive "I2I" max_from_$1 :: $2 -> BitsMax_
foreign import primitive "I2I" bits32_from_$1 :: $2 -> Bits32_
}})
