-- | Tracing library
module Debug.Trace (
  putTraceMsg -- :: String -> IO ()
, trace       -- :: String -> a -> a
, traceShow   -- :: Show a => a -> b -> b
) where
import System.IO (hPutStrLn,stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | Outputs a trace message to stderr; currently defined as
-- @putTraceMsg = hPutStrLn stderr@
putTraceMsg :: String -> IO ()
putTraceMsg = hPutStrLn stderr

-- | Tracing call
trace :: String -> a -> a
trace s e = unsafePerformIO $ do putTraceMsg s
                                 return e

-- | Like 'trace', but uses show; defined as
-- @traceShow = trace . show@
traceShow :: Show a => a -> b -> b
traceShow = trace . show
