{-# OPTIONS -w -funbox-strict-fields #-}
module Options(
    processOptions,
    Opt(..),
    options,
    Mode(..),
    putVerbose,
    putVerboseLn,
    getArguments,
    verbose,
    verbose2,
    dump,
    wdump,
    fopts,
    flint,
    fileOptions,
    withOptions,
    withOptionsT,
    getArgString,
    OptM(),
    OptT(),
    OptionMonad(..),
    flagOpt
    ) where

import Control.Monad.Error()    -- IO MonadPlus instance
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Set as S
import Data.List(nub)
import System
import System.Console.GetOpt
import System.IO.Unsafe
import System.Directory

import Data.DeriveTH
import Data.Derive.All
import Util.Gen
import qualified FlagDump
import qualified FlagOpts
import LHCVersion

data Mode = BuildHl String -- ^ Build the specified hl-file given a description file.
          | Interactive    -- ^ Run interactively.
          | Version        -- ^ Print version and die.
          | ShowHelp       -- ^ Show help message and die.
          | Interpret      -- ^ Interpret.
          | GenerateHo     -- ^ Generate ho
          | GenerateHoGrin -- ^ Generate ho and grin
          | GenerateC      -- ^ Generate C file
          | GenerateExe    -- ^ Generate executable
          | KeepTmpFiles   -- ^ Keep ho grin C and generate executable
          | DependencyTree -- ^ show simple dependency tree
          | ShowHo String  -- ^ Show ho-file.
          | ListLibraries  -- ^ List libraries
            deriving(Eq)


data Opt = Opt {
    optMode        :: Mode,       -- ^ Mode of interaction
    optColumns     :: !Int,       -- ^ Width of terminal.
    optDebug       :: !Bool,      -- ^ Debugging.
    optDump        ::  [String],  -- ^ Dump options (raw).
    optStmts       ::  [String],  -- ^ statements to execute
    optFOpts       ::  [String],  -- ^ Flag options (raw).
    optIncdirs     ::  [String],  -- ^ Include directories.
    optProgArgs    ::  [String],  -- ^ Arguments to pass to the interpreted program.
    optCCargs      ::  [String],  -- ^ Optional arguments to the C compiler.
    optHls         ::  [String],  -- ^ Load the specified hl-files (haskell libraries).
    optHlPath      ::  [String],  -- ^ Path to look for libraries.
    optIncs        ::  [String],
    optDefs        ::  [String],
    optCC          ::  String,    -- ^ C compiler.
    optHoDir       ::  Maybe FilePath,
    optHoCache     ::  Maybe FilePath,
    optArgs        ::  [String],
    optStale       ::  [String],  -- ^ treat these modules as stale
    optKeepGoing   :: !Bool,      -- ^ Keep going when encountering errors.
    optMainFunc    ::  Maybe (Bool,String),    -- ^ Entry point name for the main function.
    optArch        ::  Maybe String,           -- ^ target architecture
    optOutName     ::  String,                 -- ^ Name of output file.
    optPrelude     :: !Bool,                   -- ^ No implicit Prelude.
    optIgnoreHo    :: !Bool,                   -- ^ Ignore ho-files.
    optNoWriteHo   :: !Bool,                   -- ^ Don't write ho-files.
    optNoAuto      :: !Bool,                   -- ^ Don't autoload packages
    optFollowDeps  :: !Bool,                   -- ^ Don't follow dependencies, all deps must be loaded from packages or specified on the command line.
    optVerbose     :: !Int,                    -- ^ Verbosity
    optStatLevel   :: !Int,                    -- ^ Level to print statistics
    optDumpSet     ::  S.Set FlagDump.Flag,    -- ^ Dump flags.
    optFOptsSet    ::  S.Set FlagOpts.Flag     -- ^ Flag options (-f\<opt\>).
  } 
$(derive makeUpdate ''Opt)


opt = Opt {
    optMode        = GenerateExe,
    optColumns     = getColumns,
    optDebug       = False,
    optIncdirs     = initialIncludes,
    optHls         = [],
    optHlPath      = initialLibIncludes,
    optIncs        = [],
    optDefs        = [],
    optProgArgs    = [],
    optDump        = [],
    optStale       = [],
    optStmts       = [],
    optFOpts       = ["default"],
    optCCargs      = [],
    optHoDir       = Nothing,
    optHoCache     = Nothing,
    optCC          = "gcc",
    optArgs        = [],
    optIgnoreHo    = False,
    optNoWriteHo   = False,
    optKeepGoing   = False,
    optMainFunc    = Nothing,
    optArch        = Nothing,
    optOutName     = "hs.out",
    optPrelude     = True,
    optFollowDeps  = True,
    optVerbose     = 0,
    optStatLevel   = 1,
    optNoAuto      = False,
    optDumpSet     = S.empty,
    optFOptsSet    = S.empty
}

idu "-" _ = []
idu d ds = ds ++ [d]

theoptions :: [OptDescr (Opt -> Opt)]
theoptions =
    [ Option ['V'] ["version"]   (NoArg  (optMode_s Version))          "print version info and exit"
    , Option []    ["help"]      (NoArg  (optMode_s ShowHelp))         "print help information and exit"
    , Option ['v'] ["verbose"]   (NoArg  (optVerbose_u (+1)))          "chatty output on stderr"
    , Option ['z'] []            (NoArg  (optStatLevel_u (+1)))        "Increase verbosity of statistics"
    , Option ['d'] []            (ReqArg (\d -> optDump_u (d:)) "[no-]flag")  "dump specified data during compilation"
    , Option ['f'] []            (ReqArg (\d -> optFOpts_u (d:)) "[no-]flag") "set or clear compilation options"
    , Option ['o'] ["output"]    (ReqArg (optOutName_s) "FILE")        "output to FILE"
    , Option ['i'] ["include"]   (ReqArg (optIncdirs_u . idu) "DIR")   "where to look for source files"
    , Option ['I'] []            (ReqArg (optIncs_u . idu) "DIR")       "add to preprocessor include path"
    , Option ['D'] []            (ReqArg (\d -> optDefs_u (d:)) "NAME=VALUE") "add new definitions to set in preprocessor"
    , Option []    ["optc"]      (ReqArg (optCCargs_u . idu) "option") "extra options to pass to c compiler"
    , Option []    ["progc"]     (ReqArg (\d -> optCC_s d) "gcc")      "c compiler to use"
    , Option []    ["arg"]       (ReqArg (\d -> optProgArgs_u (++ [d])) "arg") "arguments to pass interpreted program"
    , Option ['N'] ["noprelude"] (NoArg  (optPrelude_s False))         "no implicit prelude"
    , Option ['C'] []            (NoArg  (optMode_s GenerateHo))       "Typecheck and compile ho."
    , Option ['G'] []            (NoArg  (optMode_s GenerateHoGrin))   "Typecheck, compile ho and grin."
    , Option ['c'] []            (NoArg  (optMode_s GenerateC))        "generate C but don't link"
    , Option [] ["keep-tmp-files"] (NoArg (optMode_s KeepTmpFiles))    "keep all temporary files"
    , Option []    ["interpret"] (NoArg  (optMode_s Interpret))        "interpret."
    , Option ['k'] ["keepgoing"] (NoArg  (optKeepGoing_s True))        "keep going on errors."
    , Option []    ["width"]     (ReqArg (optColumns_s . read) "COLUMNS") "width of screen for debugging output."
    , Option []    ["main"]      (ReqArg (optMainFunc_s . Just . (,) False) "Main.main")  "main entry point."
    , Option ['m'] ["arch"]      (ReqArg (optArch_s . Just ) "arch")            "target architecture."
    , Option []    ["entry"]     (ReqArg (optMainFunc_s . Just . (,) True)  "<expr>")  "main entry point, showable expression."
    , Option ['e'] []            (ReqArg (\d -> optStmts_u (d:)) "<statement>")  "run given statement as if on lhci prompt"
    , Option []    ["debug"]     (NoArg  (optDebug_s True))            "debugging"
    , Option []    ["show-ho"]   (ReqArg  (optMode_s . ShowHo) "file.ho") "Show ho file"
    , Option []    ["noauto"]    (NoArg  (optNoAuto_s True))           "Don't automatically load base and haskell98 packages"
    , Option ['p'] []            (ReqArg (\d -> optHls_u (++ [d])) "file.hl") "Load given haskell library .hl file"
    , Option ['L'] []            (ReqArg (optHlPath_u . idu) "path")   "Look for haskell libraries in the given directory."
    , Option []    ["build-hl"]  (ReqArg (optMode_s . BuildHl) "file.cabal") "Build hakell library from given library description file"
    , Option []    ["interactive"] (NoArg  (optMode_s Interactive))    "run interactivly"
    , Option []    ["ignore-ho"]   (NoArg  (optIgnoreHo_s True))       "Ignore existing haskell object files"
    , Option []    ["nowrite-ho"]  (NoArg  (optNoWriteHo_s True))      "Do not write new haskell object files"
    , Option []    ["no-ho"]       (NoArg  (optNoWriteHo_s True . optIgnoreHo_s True)) "same as --ignore-ho and --nowrite-ho"
    , Option []    ["ho-cache"]    (ReqArg (optHoCache_s . Just ) "HOCACHEDIR")    "Use a global ho cache located at the argument"
    , Option []    ["ho-dir"]      (ReqArg (optHoDir_s . Just ) "<dir>")    "Where to place and look for ho files"
    , Option []    ["stale"]       (ReqArg (optStale_u . idu) "Module")     "Treat these modules as stale, even if a ho file is present"
    , Option []    ["dependency"]  (NoArg  (optMode_s DependencyTree))  "Follow import dependencies only then quit"
    , Option []    ["no-follow-deps"] (NoArg  (optFollowDeps_s False)) "Don't follow depencies not listed on command line."
    , Option []    ["list-libraries"] (NoArg  (optMode_s ListLibraries)) "List of installed libraries."
    ]

-- | Width of terminal.
getColumns :: Int
getColumns = read $ unsafePerformIO (getEnv "COLUMNS" `mplus` return "80")


postProcessFD :: Monad m => Opt -> m Opt
postProcessFD o = case FlagDump.process (optDumpSet o) (optDump o ++ vv) of
        (s,[]) -> return $ o { optDumpSet = s }
        (_,xs) -> fail ("Unrecognized dump flag passed to '-d': "
                        ++ unwords xs ++ "\nValid dump flags:\n\n" ++ FlagDump.helpMsg)
    where
    vv | optVerbose o >= 2 = ["veryverbose"]
       | optVerbose o >= 1 = ["verbose"]
       | otherwise = []

postProcessFO :: Monad m => Opt -> m Opt
postProcessFO o = case FlagOpts.process (optFOptsSet o) (optFOpts o) of
        (s,[]) -> return $ o { optFOptsSet = s }
        (_,xs) -> fail ("Unrecognized flag passed to '-f': "
                        ++ unwords xs ++ "\nValid flags:\n\n" ++ FlagOpts.helpMsg)

getArguments = do
    x <- lookupEnv "LHCOPTS"
    let eas = maybe [] words x
    as <- System.getArgs
    return (eas ++ as)

pfill ::
    Int            -- ^ maximum width
    -> (a -> Int)  -- ^ find width of any element
    -> [a]         -- ^ input elements
    -> [[a]]       -- ^ output element
pfill maxn length xs = f maxn xs [] [] where
    f n (x:xs) ws ls | lx < n = f (n - lx) xs (x:ws) ls where
        lx = length x
    f _ (x:xs) [] ls = f (maxn - length x) xs [x] ls
    f _ (x:xs) ws ls = f (maxn - length x) xs [x] (ws:ls)
    f _ [] [] ls = reverse (map reverse ls)
    f _ [] ws ls = reverse (map reverse (ws:ls))

{-# NOINLINE processOptions #-}
-- | Parse commandline options.
processOptions :: IO Opt
processOptions = do
    argv <- getArguments
    let header = "Usage: lhc [OPTION...] Main.hs"
    let mkoptlist d os = "valid " ++ d ++ " arguments: 'help' for more info\n    " ++ intercalate "\n    " (map (intercalate ", ") $ pfill 100 ((2 +) . length) os) ++ "\n"
    let trailer = "\n" ++ mkoptlist "-d" FlagDump.helpFlags ++ "\n" ++ mkoptlist "-f" FlagOpts.helpFlags
    let (o,ns,rc) = getOpt Permute theoptions argv
    when (rc /= []) $ putErrDie (concat rc ++ usageInfo header theoptions ++ trailer)
    o1 <- either putErrDie return $ postProcessFD (foldl (flip ($)) opt o)
    o2 <- either putErrDie return $ postProcessFO o1
    when (optMode o2 == ShowHelp) $ do
        putStrLn (usageInfo header theoptions ++ trailer)
        exitSuccess
    case optNoAuto o2 of
      True -> return (o2 { optArgs = ns })
      False-> return (o2 { optArgs = ns, optHls  = ("base":optHls o2) })


{-# NOINLINE fileOptions #-}
fileOptions :: Monad m => [String] -> m Opt
fileOptions xs = case getOpt Permute theoptions xs of
    (os,[],[]) -> postProcessFD (foldl (flip ($)) options os) >>= postProcessFO
    (_,_,errs) -> fail (concat errs)

{-# NOINLINE options #-}
-- | The global options currently used.
options :: Opt
options = unsafePerformIO processOptions

-- | Put a string to stderr when running verbose.
putVerbose :: String -> IO ()
putVerbose s = when (optVerbose options > 0) $ putErr s

-- | Put a line to stderr when running verbose.
putVerboseLn :: String -> IO ()
putVerboseLn s = putVerbose (s ++ "\n")

-- | Is verbose > 0?
verbose :: Bool
verbose = optVerbose options > 0
-- | Is verbose > 1?
verbose2 :: Bool
verbose2 = optVerbose options > 1

-- | Test whether a dump flag is set.
dump :: FlagDump.Flag -> Bool
dump s = s `S.member` optDumpSet options
-- | Test whether an option flag is set.
fopts :: FlagOpts.Flag -> Bool
fopts s = s `S.member` optFOptsSet options
-- | Do the action when the suplied dump flag is set.
wdump :: (Monad m) => FlagDump.Flag -> m () -> m ()
wdump f = when (dump f)

-- | Is the \"lint\" option flag set?
flint :: Bool
flint = FlagOpts.Lint `S.member` optFOptsSet options

-- | Include directories taken from LHCPATH enviroment variable.
initialIncludes :: [String]
initialIncludes = unsafePerformIO $ do
    p <- lookupEnv "LHCPATH"
    let x = maybe "" id p
    return (".":(tokens (== ':') x))

-- | Include directories taken from LHCLIBPATH enviroment variable.
initialLibIncludes :: [String]
initialLibIncludes = unsafePerformIO $ do
    ps <- lookupEnv "LHCLIBPATH"
    h <- lookupEnv "HOME"
    l <- getAppUserDataDirectory "lhc"
    let paths = h ++ ["/usr/local","/usr"]
        bases = ["/lib","/share","/.cabal/lib"]
        vers = ["/lhc-" ++ shortVersion, "/lhc"]
    return $ nub $ maybe [] (tokens (':' ==))  ps ++ (l ++ "/lhc-" ++ shortVersion):[ p ++ b ++ v | b <- bases, p <- paths, v <- vers ]


class Monad m => OptionMonad m where
    getOptions :: m Opt
    getOptions = return options

instance OptionMonad Identity

newtype OptT m a = OptT (ReaderT Opt m a)
    deriving(MonadIO,Monad,Functor,MonadTrans)

type OptM = OptT Identity

instance Monad m => OptionMonad (OptT m) where
    getOptions = OptT ask



withOptions :: Opt -> OptM a -> a
withOptions opt (OptT x) = runIdentity (runReaderT x opt)

withOptionsT :: Opt -> OptT m a -> m a
withOptionsT opt (OptT x) = runReaderT x opt


flagOpt :: OptionMonad m => FlagOpts.Flag -> m Bool
flagOpt flag = do
    opt <- getOptions
    return (flag `S.member` optFOptsSet opt)

getArgString = do
    name <- System.getProgName
    args <- getArguments
    return (simpleQuote (name:args),head $ lines versionString)
