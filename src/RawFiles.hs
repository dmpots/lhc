module RawFiles where
import System.IO.Unsafe
import Paths_lhc

{-# NOINLINE hsffi_h #-}
hsffi_h :: String
hsffi_h = readf "HsFFI.h"

{-# NOINLINE lhc_rts_c #-}
lhc_rts_c :: String
lhc_rts_c = readf "rts/lhc_rts.c"

{-# NOINLINE lhc_rts_header_h #-}
lhc_rts_header_h :: String
lhc_rts_header_h = readf "rts/lhc_rts_header.h"

{-# NOINLINE wsize_h #-}
wsize_h :: String
wsize_h = readf "wsize.h"

{-# NOINLINE lhc_rts_alloc_c #-}
lhc_rts_alloc_c :: String
lhc_rts_alloc_c = readf "rts/lhc_rts_alloc.c"

{-# NOINLINE lhc_rts2_c #-}
lhc_rts2_c :: String
lhc_rts2_c = readf "rts/lhc_rts2.c"

{-# NOINLINE prelude_m4 #-}
prelude_m4 :: String
prelude_m4 = readf "prelude.m4"

-- convenience
readf = unsafePerformIO . readFile . unsafePerformIO . getDataFileName
