{-# LANGUAGE CPP #-}
module Main where

import Setup
import TestCase

import System.Environment
import System.Process
import System.FilePath
import System.IO
import System.Exit
import System.Directory
import Control.Monad
import Control.Exception
import System.Timeout
import Control.Concurrent
import Control.Exception
import Text.Printf
import qualified Data.ByteString.Char8 as B

data TestResult = CompileError String
                | ProgramError String String
                | KnownFailure
                | TimeOut
                | Success
                deriving Show

isSuccess Success = True
isSuccess KnownFailure = True
isSuccess _ = False

data Stats = Stats { successfulTests :: Int
                   , expectedFailures :: Int
                   , unexpectedFailures :: Int
                   , testsNotExecuted :: Int
                   }

newStats :: Int -> Stats
newStats nTests = Stats 0 0 0 nTests

successfulTest :: Stats -> Stats
successfulTest stats = stats{ successfulTests = successfulTests stats + 1
                            , testsNotExecuted = testsNotExecuted stats - 1 }

expectedFailure :: Stats -> Stats
expectedFailure stats = stats{ expectedFailures = expectedFailures stats + 1
                             , testsNotExecuted = testsNotExecuted stats - 1 }

unexpectedFailure :: Stats -> Stats
unexpectedFailure stats = stats{ unexpectedFailures = unexpectedFailures stats + 1
                               , testsNotExecuted = testsNotExecuted stats - 1 }

hasFailures :: Stats -> Bool
hasFailures stats = unexpectedFailures stats /= 0

ppStats :: Stats -> String
ppStats stats = printf ("Successful tests:    %d\n"++
                        "Expected failures:   %d\n"++
                        "Unexpected failures: %d\n"++
                        "Omitted tests:       %d\n")
                  (successfulTests stats)
                  (expectedFailures stats)
                  (unexpectedFailures stats)
                  (testsNotExecuted stats)

updateStats :: TestResult -> Stats -> Stats
updateStats Success = successfulTest
updateStats KnownFailure = expectedFailure
updateStats _ = unexpectedFailure

main :: IO ()
main = do (cfg,paths) <- parseArguments =<< getArgs
          workChan <- newChan
          resultChan <- newChan
          tests <- forM paths findTestCases
          let nTests = length (concat tests)
          writeList2Chan workChan (concat tests)
          when (cfgVerbose cfg >= 1) $ putStrLn $ "Testsuite consists of " ++ show nTests ++ " tests."
          workers <- replicateM (max 1 (cfgThreads cfg)) $ forkIO $ forever $
            do test <- readChan workChan
               result <- runTestCase cfg test
               writeChan resultChan (test,result)

          results <- getChanContents resultChan
          manager cfg (newStats nTests) (take nTests results)
            `finally` mapM_ killThread workers


errMsg = "Some tests failed to perform as expected."

manager cfg stats rest | hasFailures stats && (not (cfgComplete cfg) || null rest)
  = do when (cfgVerbose cfg == 1) $ putStrLn ""
       when (cfgVerbose cfg >= 1) $ do putStrLn errMsg
                                       putStr (ppStats stats)
       exitFailure

manager cfg stats [] | cfgVerbose cfg >= 3 = do putStrLn "No unexpected failures"
                                                putStr (ppStats stats)
manager cfg stats [] | cfgVerbose cfg >= 1 = do putStrLn ""
                                                putStr (ppStats stats)
manager cfg stats [] = return ()

manager cfg stats ((tc,result):rest)
  = do case () of () | cfgVerbose cfg >= 3 -> case result of
                                                Success      -> printf "%20s: %s\n" (testCaseName tc) "OK."
                                                KnownFailure -> printf "%20s: %s\n" (testCaseName tc) "Known failure."
                                                TimeOut      -> printf "%20s: %s\n" (testCaseName tc) "TimeOut."
                                                CompileError str | cfgVerbose cfg >= 4 -> printf "%20s: %s\n" (testCaseName tc) str
                                                ProgramError short str | cfgVerbose cfg >= 4 -> printf "%20s: %s:\n%s" (testCaseName tc) short str
                                                CompileError str -> printf "%20s: %s\n" (testCaseName tc) "Compile failure."
                                                ProgramError short str -> printf "%20s: %s\n" (testCaseName tc) short
                     | cfgVerbose cfg >= 1 -> if isSuccess result then putStr "." else putStr "*"
                     | otherwise -> return ()
       hFlush stdout
       manager cfg (updateStats result stats) rest

-- FIXME: Get a proper temporary directory.
runTestCase :: Config -> TestCase -> IO TestResult
runTestCase cfg tc
  = bracket (createDirectoryIfMissing True testDir)
            (\_ -> removeDirectoryRecursive testDir) $ \_ -> checkFail $ withTimeout $
    do let args = [ "-o", progName
                  , "--ho-dir", testDir
                  , "-flint"
                  , testCasePath tc ] ++
                  cfgLHCOptions cfg
       when (cfgVerbose cfg >= 4) $ putStrLn $ unwords (cfgLHCPath cfg:args)
       (ret,out,err) <- execProcess (cfgLHCPath cfg) args B.empty
       case ret of
         ExitFailure c -> return $ CompileError $ unlines $ ["lhc failed with: " ++ show c, B.unpack err]
         ExitSuccess
           -> do when (cfgVerbose cfg >= 4) $ putStrLn $ unwords (progName:testCaseArgs tc)
                 (ret,out,err) <- execProcess progName (testCaseArgs tc) (testCaseStdin tc)
                 case (testCaseStdout tc, testCaseStderr tc) of
                   (Just expectedOut,_) | expectedOut /= out -> return $ ProgramError "Unexpected stdout" $ B.unpack out
                   (_,Just expectedErr) | expectedErr /= err -> return $ ProgramError "Unexpected stderr" $ B.unpack err
                   _ -> return Success
  where name = dropExtension (takeFileName (testCasePath tc))
        testDir = cfgTempDir cfg </> name
        progName = testDir </> name
        checkFail io = do ret <- io
                          if testCaseMustFail tc
                             then case ret of
                                    Success -> return $ ProgramError "Known bug succeeded." ""
                                    other   -> return KnownFailure
                             else return ret
        withTimeout io = do ret <- timeout (10^6 * cfgTestTimeout cfg) io
                            case ret of
                              Nothing  -> return TimeOut
                              Just val -> return val

-- This differs from System.Process by terminating the program if an exception is raised.
execProcess :: FilePath -> [String] -> B.ByteString -> IO (ExitCode, B.ByteString, B.ByteString)
execProcess cmd args input = do
  (inh, outh, errh, pid) <- runInteractiveProcess cmd args Nothing Nothing
  handle (\e -> do terminateProcess pid
#if BASE4
                   throw (e::SomeException)) $ do
#else
                   throw e) $ do
#endif
  outVar <- newEmptyMVar
  forkIO $ B.hGetContents outh >>= putMVar outVar
  errVar <- newEmptyMVar
  forkIO $ B.hGetContents errh >>= putMVar errVar

  when (not (B.null input)) $ do B.hPutStr inh input >> hFlush inh
  hClose inh

  out <- takeMVar outVar
  err <- takeMVar errVar
  ret <- waitForProcess pid
  return (ret, out, err)
