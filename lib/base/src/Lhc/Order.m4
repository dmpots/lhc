m4_divert(-1)
m4_dnl simple macros for defining instances for classes in Lhc.Order

m4_define(INST_EQ,{{
instance Eq $1 where
    $1 x == $1 y = boxBool (equals$1 x y)
    $1 x /= $1 y = boxBool (nequals$1 x y)

foreign import primitive "Eq" equals$1 :: $2 -> $2 -> Bool__
foreign import primitive "NEq" nequals$1 :: $2 -> $2 -> Bool__

}})


m4_define(INST_ORDER,{{
instance Ord $1 where
    $1 x < $1 y = boxBool (lt$1 x y)
    $1 x > $1 y = boxBool (gt$1 x y)
    $1 x <= $1 y = boxBool (lte$1 x y)
    $1 x >= $1 y = boxBool (gte$1 x y)

foreign import primitive "$3Lt" lt$1   :: $2 -> $2 -> Bool__
foreign import primitive "$3Lte" lte$1 :: $2 -> $2 -> Bool__
foreign import primitive "$3Gt" gt$1   :: $2 -> $2 -> Bool__
foreign import primitive "$3Gte" gte$1 :: $2 -> $2 -> Bool__

}})

m4_define(INST_EQORDER,{{INST_EQ($1,$2)INST_ORDER($1,$2,$3)}})

m4_divert


