{-# OPTIONS_LHC -fcpp #-}

#ifdef __LHC__
main = putStrLn "in lhc"
#else
main = putStrLn "not in lhc"
#endif
