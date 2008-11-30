{-# LANGUAGE CPP, ScopedTypeVariables #-}
module CharIO(
    UTF8.putStr,
    UTF8.putStrLn,
    UTF8.hPutStrLn,
    putErr,
    putErrLn,
    putErrDie,
    UTF8.readFile,
    UTF8.print,
    UTF8.hGetContents,
) where
import Control.Exception as Ex
import System
import qualified IO
import qualified System.IO.UTF8 as UTF8

#if BASE4
flushOut = Ex.catch  (IO.hFlush IO.stdout) (\(e::SomeException) -> return ())
#else
flushOut = Ex.catch  (IO.hFlush IO.stdout) (\_ -> return ())
#endif

putErr    s  = flushOut >> UTF8.hPutStr   IO.stderr s
putErrLn  s  = flushOut >> UTF8.hPutStrLn IO.stderr s
putErrDie s  = flushOut >> UTF8.hPutStrLn IO.stderr s >> System.exitFailure
