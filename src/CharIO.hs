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
import Control.Exception.Extensible as Ex
import System
import qualified IO
import qualified System.IO.UTF8 as UTF8

flushOut :: IO ()
flushOut = Ex.catch  (IO.hFlush IO.stdout) (\(e::SomeException) -> return ())
putErr       :: String -> IO ()
putErr    s  = flushOut >> UTF8.hPutStr   IO.stderr s
putErrLn     :: String -> IO ()
putErrLn  s  = flushOut >> UTF8.hPutStrLn IO.stderr s
putErrDie    :: String -> IO a
putErrDie s  = flushOut >> UTF8.hPutStrLn IO.stderr s >> System.exitFailure
