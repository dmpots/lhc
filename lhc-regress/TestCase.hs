module TestCase where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import System.Directory
import System.FilePath
import Control.Monad

{-

A testcase is any Haskell file (.hs or .lhs) that has an associated
.expected.stdout or .expected.stderr file.

-}

data TestCase = TestCase { testCasePath :: String
                         , testCaseStdin  :: ByteString
                         , testCaseStdout :: Maybe ByteString
                         , testCaseStderr :: Maybe ByteString
                         , testCaseArgs   :: [String]
                         } deriving Show


findTestCases :: FilePath -> IO [TestCase]
findTestCases root
  = do contents <- getDirectoryContents root
       let walker acc [] = return acc
           walker acc (c:cs) | c `elem` [".",".."] = walker acc cs
           walker acc (c:cs)
             = do isDir <- doesDirectoryExist (root </> c)
                  if isDir
                     then do --putStrLn $ "Recursing: " ++ root </> c
                             sub <- findTestCases (root </> c)
                             walker (sub++acc) cs
                     else do --putStrLn $ "Looking at: " ++ root </> c
                             mbTest <- getTestCase (root </> c)
                             case mbTest of
                               Nothing   -> walker acc cs
                               Just test -> walker (test:acc) cs
       walker [] contents


getTestCase :: FilePath -> IO (Maybe TestCase)
getTestCase path | takeExtension path `elem` [".hs",".lhs"]
  = do isValid <- liftM2 (||) (doesFileExist stdoutFile)
                              (doesFileExist stderrFile)
       if isValid
          then do stdin  <- B.readFile stdinFile `orElse` return B.empty
                  stdout <- fmap Just (B.readFile stdoutFile) `orElse` return Nothing
                  stderr <- fmap Just (B.readFile stderrFile) `orElse` return Nothing
                  args <- fmap words (readFile argsFile) `orElse` return [] -- FIXME: Use unlines?
                  return $ Just TestCase { testCasePath = path
                                         , testCaseStdin = stdin
                                         , testCaseStdout = stdout
                                         , testCaseStderr = stderr
                                         , testCaseArgs = args }
          else return Nothing
  where root = takeDirectory path
        name = dropExtension (takeFileName path)
        stdinFile = root </> name <.> "stdin"
        stdoutFile = root </> name <.> "expected.stdout"
        stderrFile = root </> name <.> "expected.stderr"
        argsFile = root </> name <.> "args"
getTestCase _ = return Nothing

testCaseRoot = takeDirectory . testCasePath
testCaseName = dropExtension . takeFileName . testCasePath

a `orElse` b = a `catch` \_e -> b


