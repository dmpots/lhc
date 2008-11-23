{-# LANGUAGE CPP, ScopedTypeVariables, NoMonomorphismRestriction #-}
module CharIO(
    putStr,
    putStrLn,
    hPutStrLn,
    putErr,
    putErrLn,
    putErrDie,
    CharIO.readFile,
    CharIO.print,
    CharIO.hGetContents,
    runMain
    ) where

import Char
import Control.Exception
import Prelude hiding(putStr, putStrLn)
import System
import qualified IO
import qualified System.IO.UTF8 as UTF8
import qualified Prelude (putStr, putStrLn)

#if __GLASGOW_HASKELL__ >= 610
flushOut = Control.Exception.catch  (IO.hFlush IO.stdout) (\(e::SomeException) -> return ())
#else
flushOut = Control.Exception.catch  (IO.hFlush IO.stdout) (\_ -> return ())
#endif

putStr       = UTF8.putStr
putStrLn     = UTF8.putStrLn
putErr    s  = flushOut >> UTF8.hPutStr IO.stderr s
putErrLn  s  = flushOut >> UTF8.hPutStrLn IO.stderr s
putErrDie s  = flushOut >> UTF8.hPutStrLn IO.stderr s >> System.exitFailure
print        = UTF8.print
hPutStrLn    = UTF8.hPutStrLn
readFile     = UTF8.readFile
hGetContents = UTF8.hGetContents

runMain :: IO a -> IO ()
#if __GLASGOW_HASKELL__ >= 610
runMain action = Control.Exception.catches (action >> return ())
                   [ Handler $ \ (e::ExitCode) -> throw e
                   , Handler $ \ (e::SomeException) -> putErrDie $ show e ]
#else
runMain action = Control.Exception.catch (action >> return ()) $ \e ->
                   case e of
                     ExitException c -> throw (ExitException c)
                     other -> putErrDie $ show e
#endif
