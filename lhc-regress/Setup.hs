module Setup
  ( Config(..)
  , parseArguments
  ) where

import System.Exit
import System.Environment
import System.Console.GetOpt
import System.Directory
import Data.Maybe
import Data.Char

data Config =
  Config { cfgShowMarginalCoverage :: Bool
         , cfgVerbose              :: Int
         , cfgThreads              :: Int
         , cfgTimeLimit            :: Maybe Int
         , cfgTempDir              :: FilePath
         , cfgLHCPath              :: FilePath
         , cfgLHCOptions           :: [String]
         , cfgTestTimeout          :: Int -- in seconds
         } deriving Show


emptyConfig :: IO Config
emptyConfig = do tmp <- getTemporaryDirectory
                 lhc <- findExecutable "lhc"
                 return Config { cfgShowMarginalCoverage = False
                               , cfgVerbose = 1
                               , cfgThreads = 1
                               , cfgTimeLimit = Nothing
                               , cfgTempDir = tmp
                               , cfgLHCPath = fromMaybe "lhc" lhc
                               , cfgLHCOptions = ["+RTS","-M1G","-RTS"]
                               , cfgTestTimeout = 120 }

data Flag
    = HelpFlag
    | Verbose Int
    | DryRun Bool
    | Threads Int
    | Options [String]
    | TimeLimit Int
      deriving Show


cmd_verbose :: OptDescr (Config -> Config)
cmd_verbose = Option "v" ["verbose"] (OptArg verboseFlag "n")
              "Control verbosity (n is 0-5, normal verbosity level is 1, -v alone is equivalent to -v3)"
  where
    verboseFlag mb_s cfg = cfg{cfgVerbose = (maybe 3 read mb_s)}

cmd_threads :: OptDescr (Config -> Config)
cmd_threads = Option "N" ["threads"] (ReqArg threadsFlag "n")
              "Use <n> OS threads (default: 1)"
  where
    threadsFlag s cfg = cfg{cfgThreads = read s }

cmd_options :: OptDescr (Config -> Config)
cmd_options = Option "" ["lhc-options"] (ReqArg optionsFlag "OPTS")
              "give extra options to lhc"
  where
    optionsFlag s cfg = cfg{cfgLHCOptions = words s ++ cfgLHCOptions cfg}

{-
cmd_dryrun :: OptDescr Flag
cmd_dryrun = Option "d" ["dry-run"] (OptArg dryrunFlag "bool")
              "Dry run. Accept values in the line of 'false', '0' and 'no'. Default: false."
  where
    dryrunFlag mb_s = DryRun (maybe True (parse.map toLower) mb_s)
    parse "false" = False
    parse "0" = False
    parse "no" = False
    parse _ = True
-}

globalOptions :: [OptDescr (Config -> Config)]
globalOptions =
    [-- cmd_help
      cmd_verbose
    , cmd_threads
    , cmd_options
--    , cmd_dryrun
    ]


printUsage =
    do pname <- getProgName
       let syntax_line = concat [ "Usage: ", pname
                                , " [FLAGS] [PATH]"
                                , "\n\nGlobal flags:"]
       putStrLn (usageInfo syntax_line globalOptions)
  where align n str = str ++ replicate (n - length str) ' '

parseArguments :: [String] -> IO (Config, [FilePath])
parseArguments args
  = case getOpt' RequireOrder globalOptions args of
      (flags,paths,[],[]) ->
         do cfg <- emptyConfig
            return (foldr (.) id flags cfg, if null paths then ["."] else paths)
      (flags,paths,warns,[]) ->
         do printUsage
            exitWith ExitSuccess
      (_,_,_,errs) ->
         do putStrLn $ "Errors: \n" ++ unlines errs
            exitFailure
