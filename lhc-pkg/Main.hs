{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004-2009.
--
-- Package management tool
--
-----------------------------------------------------------------------------

module Main (main) where

import Distribution.InstalledPackageInfo.Binary
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.ModuleName hiding (main)
import Distribution.InstalledPackageInfo
import Distribution.InstalledPackageInfo.Binary
import Distribution.Compat.ReadP
import Distribution.ParseUtils
import Distribution.Package hiding (depends)
import Distribution.Text
import Distribution.Version
import System.FilePath
import System.Cmd       ( rawSystem )
import System.Info as SysVer
import System.Directory ( getAppUserDataDirectory, createDirectoryIfMissing,
                          getModificationTime )
import Text.Printf
import qualified Version 

import Prelude


import System.Console.GetOpt
#if __GLASGOW_HASKELL__ >= 609
import qualified Control.Exception as Exception
#else
import qualified Control.Exception.Extensible as Exception
#endif
import Data.Maybe

import Data.Char ( isSpace, toLower )
import Control.Monad
import System.Directory ( doesDirectoryExist, getDirectoryContents,
                          doesFileExist, renameFile, removeFile )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs, getProgName, getEnv )
import System.IO
import System.IO.Error (try)
import Data.List
import Control.Concurrent

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin

import Foreign
import Foreign.C
#ifdef mingw32_HOST_OS
import GHC.ConsoleHandler
#else
import System.Posix hiding (fdToHandle)
#endif

import IO ( isPermissionError )
import System.Posix.Internals
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO.Handle.FD (fdToHandle)
#else
import GHC.Handle (fdToHandle)
#endif

#if defined(GLOB)
import System.Process(runInteractiveCommand)
import qualified System.Info(os)
#endif

#if !defined(mingw32_HOST_OS) && __GLASGOW_HASKELL__ >= 611 && !defined(BOOTSTRAPPING)
import System.Console.Terminfo as Terminfo
#endif

-- -----------------------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
  args <- getArgs

  case getOpt Permute (flags ++ deprecFlags) args of
        (cli,_,[]) | FlagHelp `elem` cli -> do
           prog <- getProgramName
           bye (usageInfo (usageHeader prog) flags)
        (cli,_,[]) | FlagVersion `elem` cli ->
           bye ourCopyright
        (cli,nonopts,[]) ->
           case getVerbosity Normal cli of
           Right v -> runit v cli nonopts
           Left err -> die err
        (_,_,errors) -> do
           prog <- getProgramName
           die (concat errors ++ usageInfo (usageHeader prog) flags)

-- -----------------------------------------------------------------------------
-- Command-line syntax

data Flag
  = FlagUser
  | FlagGlobal
  | FlagHelp
  | FlagVersion
  | FlagConfig FilePath
  | FlagGlobalConfig FilePath
  | FlagForce
  | FlagForceFiles
  | FlagAutoGHCiLibs
  | FlagSimpleOutput
  | FlagNamesOnly
  | FlagIgnoreCase
  | FlagNoUserDb
  | FlagVerbosity (Maybe String)
  deriving Eq

flags :: [OptDescr Flag]
flags = [
  Option [] ["user"] (NoArg FlagUser)
        "use the current user's package database",
  Option [] ["global"] (NoArg FlagGlobal)
        "use the global package database",
  Option ['f'] ["package-conf"] (ReqArg FlagConfig "FILE")
        "use the specified package config file",
  Option [] ["global-conf"] (ReqArg FlagGlobalConfig "FILE")
        "location of the global package config",
  Option [] ["no-user-package-conf"] (NoArg FlagNoUserDb)
        "never read the user package database",
  Option [] ["force"] (NoArg FlagForce)
         "ignore missing dependencies, directories, and libraries",
  Option [] ["force-files"] (NoArg FlagForceFiles)
         "ignore missing directories and libraries only",
  Option ['g'] ["auto-ghci-libs"] (NoArg FlagAutoGHCiLibs)
        "automatically build libs for GHCi (with register)",
  Option ['?'] ["help"] (NoArg FlagHelp)
        "display this help and exit",
  Option ['V'] ["version"] (NoArg FlagVersion)
        "output version information and exit",
  Option [] ["simple-output"] (NoArg FlagSimpleOutput)
        "print output in easy-to-parse format for some commands",
  Option [] ["names-only"] (NoArg FlagNamesOnly)
        "only print package names, not versions; can only be used with list --simple-output",
  Option [] ["ignore-case"] (NoArg FlagIgnoreCase)
        "ignore case for substring matching",
  Option ['v'] ["verbose"] (OptArg FlagVerbosity "Verbosity")
        "verbosity level (0-2, default 1)"
  ]

data Verbosity = Silent | Normal | Verbose
    deriving (Show, Eq, Ord)

getVerbosity :: Verbosity -> [Flag] -> Either String Verbosity
getVerbosity v [] = Right v
getVerbosity _ (FlagVerbosity Nothing    : fs) = getVerbosity Verbose fs
getVerbosity _ (FlagVerbosity (Just "0") : fs) = getVerbosity Silent  fs
getVerbosity _ (FlagVerbosity (Just "1") : fs) = getVerbosity Normal  fs
getVerbosity _ (FlagVerbosity (Just "2") : fs) = getVerbosity Verbose fs
getVerbosity _ (FlagVerbosity v : _) = Left ("Bad verbosity: " ++ show v)
getVerbosity v (_ : fs) = getVerbosity v fs

deprecFlags :: [OptDescr Flag]
deprecFlags = [
        -- put deprecated flags here
  ]

ourCopyright :: String
ourCopyright = "LHC package manager version " ++ Version.version ++ "\n"

usageHeader :: String -> String
usageHeader prog = substProg prog $
  "Usage:\n" ++
  "  $p init {path}\n" ++
  "    Create and initialise a package database at the location {path}.\n" ++
  "    Packages can be registered in the new database using the register\n" ++
  "    command with --package-conf={path}.  To use the new database with LHC,\n" ++
  "    use LHC's -package-conf flag.\n" ++
  "\n" ++
  "  $p register {filename | -}\n" ++
  "    Register the package using the specified installed package\n" ++
  "    description. The syntax for the latter is given in the $p\n" ++
  "    documentation.\n" ++
  "\n" ++
  "  $p update {filename | -}\n" ++
  "    Register the package, overwriting any other package with the\n" ++
  "    same name.\n" ++
  "\n" ++
  "  $p unregister {pkg-id}\n" ++
  "    Unregister the specified package.\n" ++
  "\n" ++
  "  $p expose {pkg-id}\n" ++
  "    Expose the specified package.\n" ++
  "\n" ++
  "  $p hide {pkg-id}\n" ++
  "    Hide the specified package.\n" ++
  "\n" ++
  "  $p list [pkg]\n" ++
  "    List registered packages in the global database, and also the\n" ++
  "    user database if --user is given. If a package name is given\n" ++
  "    all the registered versions will be listed in ascending order.\n" ++
  "    Accepts the --simple-output flag.\n" ++
  "\n" ++
  "  $p dot\n" ++
  "    Generate a graph of the package dependencies in a form suitable\n" ++
  "    for input for the graphviz tools.  For example, to generate a PDF" ++
  "    of the dependency graph: lhc-pkg dot | tred | dot -Tpdf >pkgs.pdf" ++
  "\n" ++
  "  $p find-module {module}\n" ++
  "    List registered packages exposing module {module} in the global\n" ++
  "    database, and also the user database if --user is given.\n" ++
  "    All the registered versions will be listed in ascending order.\n" ++
  "    Accepts the --simple-output flag.\n" ++
  "\n" ++
  "  $p latest {pkg-id}\n" ++
  "    Prints the highest registered version of a package.\n" ++
  "\n" ++
  "  $p check\n" ++
  "    Check the consistency of package depenencies and list broken packages.\n" ++
  "    Accepts the --simple-output flag.\n" ++
  "\n" ++
  "  $p describe {pkg}\n" ++
  "    Give the registered description for the specified package. The\n" ++
  "    description is returned in precisely the syntax required by $p\n" ++
  "    register.\n" ++
  "\n" ++
  "  $p field {pkg} {field}\n" ++
  "    Extract the specified field of the package description for the\n" ++
  "    specified package. Accepts comma-separated multiple fields.\n" ++
  "\n" ++
  "  $p dump\n" ++
  "    Dump the registered description for every package.  This is like\n" ++
  "    \"lhc-pkg describe '*'\", except that it is intended to be used\n" ++
  "    by tools that parse the results, rather than humans.\n" ++
  "\n" ++
  " Substring matching is supported for {module} in find-module and\n" ++
  " for {pkg} in list, describe, and field, where a '*' indicates\n" ++
  " open substring ends (prefix*, *suffix, *infix*).\n" ++
  "\n" ++
  "  When asked to modify a database (register, unregister, update,\n"++
  "  hide, expose, and also check), lhc-pkg modifies the global database by\n"++
  "  default.  Specifying --user causes it to act on the user database,\n"++
  "  or --package-conf can be used to act on another database\n"++
  "  entirely. When multiple of these options are given, the rightmost\n"++
  "  one is used as the database to act upon.\n"++
  "\n"++
  "  Commands that query the package database (list, tree, latest, describe,\n"++
  "  field) operate on the list of databases specified by the flags\n"++
  "  --user, --global, and --package-conf.  If none of these flags are\n"++
  "  given, the default is --global --user.\n"++
  "\n" ++
  " The following optional flags are also accepted:\n"

substProg :: String -> String -> String
substProg _ [] = []
substProg prog ('$':'p':xs) = prog ++ substProg prog xs
substProg prog (c:xs) = c : substProg prog xs

-- -----------------------------------------------------------------------------
-- Do the business

data Force = NoForce | ForceFiles | ForceAll | CannotForce
  deriving (Eq,Ord)

data PackageArg = Id PackageIdentifier | Substring String (String->Bool)

runit :: Verbosity -> [Flag] -> [String] -> IO ()
runit verbosity cli nonopts = do
  installSignalHandlers -- catch ^C and clean up
  prog <- getProgramName
  let
        force
          | FlagForce `elem` cli        = ForceAll
          | FlagForceFiles `elem` cli   = ForceFiles
          | otherwise                   = NoForce
        auto_ghci_libs = FlagAutoGHCiLibs `elem` cli
        splitFields fields = unfoldr splitComma (',':fields)
          where splitComma "" = Nothing
                splitComma fs = Just $ break (==',') (tail fs)

        substringCheck :: String -> Maybe (String -> Bool)
        substringCheck ""    = Nothing
        substringCheck "*"   = Just (const True)
        substringCheck [_]   = Nothing
        substringCheck (h:t) =
          case (h, init t, last t) of
            ('*',s,'*') -> Just (isInfixOf (f s) . f)
            ('*',_, _ ) -> Just (isSuffixOf (f t) . f)
            ( _ ,s,'*') -> Just (isPrefixOf (f (h:s)) . f)
            _           -> Nothing
          where f | FlagIgnoreCase `elem` cli = map toLower
                  | otherwise                 = id
#if defined(GLOB)
        glob x | System.Info.os=="mingw32" = do
          -- glob echoes its argument, after win32 filename globbing
          (_,o,_,_) <- runInteractiveCommand ("glob "++x)
          txt <- hGetContents o
          return (read txt)
        glob x | otherwise = return [x]
#endif
  --
  -- first, parse the command
  case nonopts of
#if defined(GLOB)
    -- dummy command to demonstrate usage and permit testing
    -- without messing things up; use glob to selectively enable
    -- windows filename globbing for file parameters
    -- register, update, FlagGlobalConfig, FlagConfig; others?
    ["glob", filename] -> do
        print filename
        glob filename >>= print
#endif
    ["init", filename] ->
        initPackageDB filename verbosity cli
    ["register", filename] ->
        registerPackage filename verbosity cli auto_ghci_libs False force
    ["update", filename] ->
        registerPackage filename verbosity cli auto_ghci_libs True force
    ["unregister", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        unregisterPackage pkgid verbosity cli force
    ["expose", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        exposePackage pkgid verbosity cli force
    ["hide",   pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        hidePackage pkgid verbosity cli force
    ["list"] -> do
        listPackages verbosity cli Nothing Nothing
    ["list", pkgid_str] ->
        case substringCheck pkgid_str of
          Nothing -> do pkgid <- readGlobPkgId pkgid_str
                        listPackages verbosity cli (Just (Id pkgid)) Nothing
          Just m -> listPackages verbosity cli (Just (Substring pkgid_str m)) Nothing
    ["dot"] -> do
        showPackageDot verbosity cli
    ["find-module", moduleName] -> do
        let match = maybe (==moduleName) id (substringCheck moduleName)
        listPackages verbosity cli Nothing (Just match)
    ["latest", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        latestPackage verbosity cli pkgid
    ["describe", pkgid_str] ->
        case substringCheck pkgid_str of
          Nothing -> do pkgid <- readGlobPkgId pkgid_str
                        describePackage verbosity cli (Id pkgid)
          Just m -> describePackage verbosity cli (Substring pkgid_str m)
    ["field", pkgid_str, fields] ->
        case substringCheck pkgid_str of
          Nothing -> do pkgid <- readGlobPkgId pkgid_str
                        describeField verbosity cli (Id pkgid) 
                                      (splitFields fields)
          Just m -> describeField verbosity cli (Substring pkgid_str m)
                                      (splitFields fields)
    ["check"] -> do
        checkConsistency verbosity cli

    ["dump"] -> do
        dumpPackages verbosity cli

    ["recache"] -> do
        recache verbosity cli

    [] -> do
        die ("missing command\n" ++
                usageInfo (usageHeader prog) flags)
    (_cmd:_) -> do
        die ("command-line syntax error\n" ++
                usageInfo (usageHeader prog) flags)

parseCheck :: ReadP a a -> String -> String -> IO a
parseCheck parser str what =
  case [ x | (x,ys) <- readP_to_S parser str, all isSpace ys ] of
    [x] -> return x
    _ -> die ("cannot parse \'" ++ str ++ "\' as a " ++ what)

readGlobPkgId :: String -> IO PackageIdentifier
readGlobPkgId str = parseCheck parseGlobPackageId str "package identifier"

parseGlobPackageId :: ReadP r PackageIdentifier
parseGlobPackageId =
  parse
     +++
  (do n <- parse
      _ <- string "-*"
      return (PackageIdentifier{ pkgName = n, pkgVersion = globVersion }))

-- globVersion means "all versions"
globVersion :: Version
globVersion = Version{ versionBranch=[], versionTags=["*"] }

-- -----------------------------------------------------------------------------
-- Package databases

-- Some commands operate on a single database:
--      register, unregister, expose, hide
-- however these commands also check the union of the available databases
-- in order to check consistency.  For example, register will check that
-- dependencies exist before registering a package.
--
-- Some commands operate  on multiple databases, with overlapping semantics:
--      list, describe, field

data PackageDB 
  = PackageDB { location :: FilePath,
                packages :: [InstalledPackageInfo] }

type PackageDBStack = [PackageDB]
        -- A stack of package databases.  Convention: head is the topmost
        -- in the stack.

allPackagesInStack :: PackageDBStack -> [InstalledPackageInfo]
allPackagesInStack = concatMap packages

getPkgDatabases :: Verbosity
                -> Bool    -- we are modifying, not reading
                -> Bool    -- read caches, if available
                -> [Flag]
                -> IO (PackageDBStack, 
                          -- the real package DB stack: [global,user] ++ 
                          -- DBs specified on the command line with -f.
                       Maybe FilePath,
                          -- which one to modify, if any
                       PackageDBStack)
                          -- the package DBs specified on the command
                          -- line, or [global,user] otherwise.  This
                          -- is used as the list of package DBs for
                          -- commands that just read the DB, such as 'list'.

getPkgDatabases verbosity modify use_cache my_flags = do
  -- first we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-config flag by the
  -- wrapper script.
  let err_msg = "missing --global-conf option, location of global package.conf unknown\n"
  global_conf <-
     case [ f | FlagGlobalConfig f <- my_flags ] of
        [] -> do mb_dir <- getLibDir
                 case mb_dir of
                   Nothing  -> die err_msg
                   Just dir -> do
                     r <- lookForPackageDBIn dir
                     case r of
                       Nothing -> die ("Can't find package database in " ++ dir)
                       Just path -> return path
        fs -> return (last fs)

  let no_user_db = FlagNoUserDb `elem` my_flags

  -- get the location of the user package database, and create it if necessary
  -- getAppUserDataDirectory can fail (e.g. if $HOME isn't set)
  e_appdir <- try $ getAppUserDataDirectory "lhc"

  mb_user_conf <-
     if no_user_db then return Nothing else
     case e_appdir of
       Left _    -> return Nothing
       Right appdir -> do
         let subdir = (SysVer.arch++"-"++SysVer.os++"-"++Version.version)
         --targetARCH ++ '-':targetOS ++ '-':Version.version
             dir = appdir </> subdir
         r <- lookForPackageDBIn dir
         case r of
           Nothing -> return (Just (dir </> "package.conf.d", False))
           Just f  -> return (Just (f, True))

  -- If the user database doesn't exist, and this command isn't a
  -- "modify" command, then we won't attempt to create or use it.
  let sys_databases
        | Just (user_conf,user_exists) <- mb_user_conf,
          modify || user_exists = [user_conf, global_conf]
        | otherwise             = [global_conf]

  e_pkg_path <- try (System.Environment.getEnv "LHC_PACKAGE_PATH")
  let env_stack =
        case e_pkg_path of
                Left  _ -> sys_databases
                Right path
                  | last cs == ""  -> init cs ++ sys_databases
                  | otherwise      -> cs
                  where cs = parseSearchPath path

        -- The "global" database is always the one at the bottom of the stack.
        -- This is the database we modify by default.
      virt_global_conf = last env_stack

  let db_flags = [ f | Just f <- map is_db_flag my_flags ]
         where is_db_flag FlagUser
                      | Just (user_conf, _user_exists) <- mb_user_conf 
                      = Just user_conf
               is_db_flag FlagGlobal     = Just virt_global_conf
               is_db_flag (FlagConfig f) = Just f
               is_db_flag _              = Nothing

  let flag_db_names | null db_flags = env_stack
                    | otherwise     = reverse (nub db_flags)

  -- For a "modify" command, treat all the databases as
  -- a stack, where we are modifying the top one, but it
  -- can refer to packages in databases further down the
  -- stack.

  -- -f flags on the command line add to the database
  -- stack, unless any of them are present in the stack
  -- already.
  let final_stack = filter (`notElem` env_stack)
                     [ f | FlagConfig f <- reverse my_flags ]
                     ++ env_stack

  -- the database we actually modify is the one mentioned
  -- rightmost on the command-line.
  let to_modify
        | not modify    = Nothing
        | null db_flags = Just virt_global_conf
        | otherwise     = Just (last db_flags)

  db_stack <- mapM (readParseDatabase verbosity mb_user_conf use_cache) final_stack

  let flag_db_stack = [ db | db_name <- flag_db_names,
                        db <- db_stack, location db == db_name ]

  return (db_stack, to_modify, flag_db_stack)


lookForPackageDBIn :: FilePath -> IO (Maybe FilePath)
lookForPackageDBIn dir = do
  let path_dir = dir </> "package.conf.d"
  exists_dir <- doesDirectoryExist path_dir
  if exists_dir then return (Just path_dir) else do
  let path_file = dir </> "package.conf"
  exists_file <- doesFileExist path_file
  if exists_file then return (Just path_file) else return Nothing

readParseDatabase :: Verbosity
                  -> Maybe (FilePath,Bool)
                  -> Bool -- use cache
                  -> FilePath
                  -> IO PackageDB

readParseDatabase verbosity mb_user_conf use_cache path
  -- the user database (only) is allowed to be non-existent
  | Just (user_conf,False) <- mb_user_conf, path == user_conf
  = return PackageDB { location = path, packages = [] }
  | otherwise
  = do e <- try $ getDirectoryContents path
       case e of
         Left _   -> do
              pkgs <- parseMultiPackageConf verbosity path
              return PackageDB{ location = path, packages = pkgs }              
         Right fs
           | not use_cache -> ignore_cache
           | otherwise -> do
              let cache = path </> cachefilename
              tdir     <- getModificationTime path
              e_tcache <- try $ getModificationTime cache
              case e_tcache of
                Left ex -> do
                     when (verbosity > Normal) $
                        putStrLn ("warning: cannot read cache file " ++ cache ++ ": " ++ show ex)
                     ignore_cache
                Right tcache
                  | tcache >= tdir -> do
                     when (verbosity > Normal) $
                        putStrLn ("using cache: " ++ cache)
                     pkgs <- myReadBinPackageDB cache
                     let pkgs' = map convertPackageInfoIn pkgs
                     return PackageDB { location = path, packages = pkgs' }
                  | otherwise -> do
                     when (verbosity >= Normal) $ do
                        putStrLn ("WARNING: cache is out of date: " ++ cache)
                        putStrLn "  use 'lhc-pkg recache' to fix."
                     ignore_cache
            where
                 ignore_cache = do
                     let confs = filter (".conf" `isSuffixOf`) fs
                     pkgs <- mapM (parseSingletonPackageConf verbosity) $
                                   map (path </>) confs
                     return PackageDB { location = path, packages = pkgs }

-- read the package.cache file strictly, to work around a problem with
-- bytestring 0.9.0.x (fixed in 0.9.1.x) where the file wasn't closed
-- after it has been completely read, leading to a sharing violation
-- later.
myReadBinPackageDB :: FilePath -> IO [InstalledPackageInfoString]
myReadBinPackageDB filepath = do
  h <- openBinaryFile filepath ReadMode
  sz <- hFileSize h
  b <- B.hGet h (fromIntegral sz)
  hClose h
  return $ Bin.runGet Bin.get b

parseMultiPackageConf :: Verbosity -> FilePath -> IO [InstalledPackageInfo]
parseMultiPackageConf verbosity file = do
  when (verbosity > Normal) $ putStrLn ("reading package database: " ++ file)
  str <- readFile file
  let pkgs = map convertPackageInfoIn $ read str
  Exception.evaluate pkgs
    `catchError` \e->
       die ("error while parsing " ++ file ++ ": " ++ show e)
  
parseSingletonPackageConf :: Verbosity -> FilePath -> IO InstalledPackageInfo
parseSingletonPackageConf verbosity file = do
  when (verbosity > Normal) $ putStrLn ("reading package config: " ++ file)
  readFile file >>= parsePackageInfo

cachefilename :: FilePath
cachefilename = "package.cache"

-- -----------------------------------------------------------------------------
-- Creating a new package DB

initPackageDB :: FilePath -> Verbosity -> [Flag] -> IO ()
initPackageDB filename verbosity _flags = do
  let eexist = die ("cannot create: " ++ filename ++ " already exists")
  b1 <- doesFileExist filename
  when b1 eexist
  b2 <- doesDirectoryExist filename
  when b2 eexist
  changeDB verbosity [] PackageDB{ location = filename, packages = [] }

-- -----------------------------------------------------------------------------
-- Registering

registerPackage :: FilePath
                -> Verbosity
                -> [Flag]
                -> Bool              -- auto_ghci_libs
                -> Bool              -- update
                -> Force
                -> IO ()
registerPackage input verbosity my_flags auto_ghci_libs update force = do
  (db_stack, Just to_modify, _flag_dbs) <- 
      getPkgDatabases verbosity True True my_flags

  let
        db_to_operate_on = my_head "register" $
                           filter ((== to_modify).location) db_stack
  --
  s <-
    case input of
      "-" -> do
        when (verbosity >= Normal) $
            putStr "Reading package info from stdin ... "
        getContents
      f   -> do
        when (verbosity >= Normal) $
            putStr ("Reading package info from " ++ show f ++ " ... ")
        readFile f

  expanded <- expandEnvVars s force

  pkg <- parsePackageInfo expanded
  when (verbosity >= Normal) $
      putStrLn "done."

  let truncated_stack = dropWhile ((/= to_modify).location) db_stack
  -- truncate the stack for validation, because we don't allow
  -- packages lower in the stack to refer to those higher up.
  validatePackageConfig pkg truncated_stack auto_ghci_libs update force
  let 
     removes = [ RemovePackage p
               | p <- packages db_to_operate_on,
                 sourcePackageId p == sourcePackageId pkg ]
  --
  changeDB verbosity (removes ++ [AddPackage pkg]) db_to_operate_on

parsePackageInfo
        :: String
        -> IO InstalledPackageInfo
parsePackageInfo str =
  case parseInstalledPackageInfo str of
    ParseOk _warns ok -> return ok
    ParseFailed err -> case locatedErrorMsg err of
                           (Nothing, s) -> die s
                           (Just l, s) -> die (show l ++ ": " ++ s)

-- -----------------------------------------------------------------------------
-- Making changes to a package database

data DBOp = RemovePackage InstalledPackageInfo
          | AddPackage    InstalledPackageInfo
          | ModifyPackage InstalledPackageInfo

changeDB :: Verbosity -> [DBOp] -> PackageDB -> IO ()
changeDB verbosity cmds db = do
  let db' = updateInternalDB db cmds
  isfile <- doesFileExist (location db)
  if isfile
     then writeNewConfig verbosity (location db') (packages db')
     else do
       createDirectoryIfMissing True (location db)
       changeDBDir verbosity cmds db'

updateInternalDB :: PackageDB -> [DBOp] -> PackageDB
updateInternalDB db cmds = db{ packages = foldl do_cmd (packages db) cmds }
 where
  do_cmd pkgs (RemovePackage p) = 
    filter ((/= installedPackageId p) . installedPackageId) pkgs
  do_cmd pkgs (AddPackage p) = p : pkgs
  do_cmd pkgs (ModifyPackage p) = 
    do_cmd (do_cmd pkgs (RemovePackage p)) (AddPackage p)
    

changeDBDir :: Verbosity -> [DBOp] -> PackageDB -> IO ()
changeDBDir verbosity cmds db = do
  mapM_ do_cmd cmds
  updateDBCache verbosity db
 where
  do_cmd (RemovePackage p) = do
    let file = location db </> display (installedPackageId p) <.> "conf"
    when (verbosity > Normal) $ putStrLn ("removing " ++ file)
    removeFile file
  do_cmd (AddPackage p) = do
    let file = location db </> display (installedPackageId p) <.> "conf"
    when (verbosity > Normal) $ putStrLn ("writing " ++ file)
    writeFileAtomic file (showInstalledPackageInfo p)
  do_cmd (ModifyPackage p) = 
    do_cmd (AddPackage p)

updateDBCache :: Verbosity -> PackageDB -> IO ()
updateDBCache verbosity db = do
  let filename = location db </> cachefilename
  when (verbosity > Normal) $
      putStrLn ("writing cache " ++ filename)
  writeBinPackageDB filename (map convertPackageInfoOut (packages db))
    `catch` \e ->
      if isPermissionError e
      then die (filename ++ ": you don't have permission to modify this file")
      else ioError e

-- -----------------------------------------------------------------------------
-- Exposing, Hiding, Unregistering are all similar

exposePackage :: PackageIdentifier -> Verbosity -> [Flag] -> Force -> IO ()
exposePackage = modifyPackage (\p -> ModifyPackage p{exposed=True})

hidePackage :: PackageIdentifier -> Verbosity -> [Flag] -> Force -> IO ()
hidePackage = modifyPackage (\p -> ModifyPackage p{exposed=False})

unregisterPackage :: PackageIdentifier -> Verbosity -> [Flag] -> Force -> IO ()
unregisterPackage = modifyPackage RemovePackage

modifyPackage
  :: (InstalledPackageInfo -> DBOp)
  -> PackageIdentifier
  -> Verbosity
  -> [Flag]
  -> Force
  -> IO ()
modifyPackage fn pkgid verbosity my_flags force = do
  (db_stack, Just _to_modify, _flag_dbs) <- 
      getPkgDatabases verbosity True{-modify-} True{-use cache-} my_flags

  (db, ps) <- fmap head $ findPackagesByDB db_stack (Id pkgid)
  let 
      db_name = location db
      pkgs    = packages db

      pids = map sourcePackageId ps

      cmds = [ fn pkg | pkg <- pkgs, sourcePackageId pkg `elem` pids ]
      new_db = updateInternalDB db cmds

      old_broken = brokenPackages (allPackagesInStack db_stack)
      rest_of_stack = filter ((/= db_name) . location) db_stack
      new_stack = new_db : rest_of_stack
      new_broken = map sourcePackageId (brokenPackages (allPackagesInStack new_stack))
      newly_broken = filter (`notElem` map sourcePackageId old_broken) new_broken
  --
  when (not (null newly_broken)) $
      dieOrForceAll force ("unregistering " ++ display pkgid ++
           " would break the following packages: "
              ++ unwords (map display newly_broken))

  changeDB verbosity cmds db

recache :: Verbosity -> [Flag] -> IO ()
recache verbosity my_flags = do
  (db_stack, Just to_modify, _flag_dbs) <- 
     getPkgDatabases verbosity True{-modify-} False{-no cache-} my_flags
  let
        db_to_operate_on = my_head "recache" $
                           filter ((== to_modify).location) db_stack
  --
  changeDB verbosity [] db_to_operate_on

-- -----------------------------------------------------------------------------
-- Listing packages

listPackages ::  Verbosity -> [Flag] -> Maybe PackageArg
             -> Maybe (String->Bool)
             -> IO ()
listPackages verbosity my_flags mPackageName mModuleName = do
  let simple_output = FlagSimpleOutput `elem` my_flags
  (db_stack, _, flag_db_stack) <- 
     getPkgDatabases verbosity False True{-use cache-} my_flags

  let db_stack_filtered -- if a package is given, filter out all other packages
        | Just this <- mPackageName =
            [ db{ packages = filter (this `matchesPkg`) (packages db) }
            | db <- flag_db_stack ]
        | Just match <- mModuleName = -- packages which expose mModuleName
            [ db{ packages = filter (match `exposedInPkg`) (packages db) }
            | db <- flag_db_stack ]
        | otherwise = flag_db_stack

      db_stack_sorted
          = [ db{ packages = sort_pkgs (packages db) }
            | db <- db_stack_filtered ]
          where sort_pkgs = sortBy cmpPkgIds
                cmpPkgIds pkg1 pkg2 =
                   case pkgName p1 `compare` pkgName p2 of
                        LT -> LT
                        GT -> GT
                        EQ -> pkgVersion p1 `compare` pkgVersion p2
                   where (p1,p2) = (sourcePackageId pkg1, sourcePackageId pkg2)

      stack = reverse db_stack_sorted

      match `exposedInPkg` pkg = any match (map display $ exposedModules pkg)

      pkg_map = allPackagesInStack db_stack
      broken = map sourcePackageId (brokenPackages pkg_map)

      show_normal PackageDB{ location = db_name, packages = pkg_confs } =
          hPutStrLn stdout $ unlines ((db_name ++ ":") : map ("    " ++) pp_pkgs)
           where
                 pp_pkgs = map pp_pkg pkg_confs
                 pp_pkg p
                   | sourcePackageId p `elem` broken = printf "{%s}" doc
                   | exposed p = doc
                   | otherwise = printf "(%s)" doc
                   where doc | verbosity >= Verbose = printf "%s (%s)" pkg ipid
                             | otherwise            = pkg
                          where
                          InstalledPackageId ipid = installedPackageId p
                          pkg = display (sourcePackageId p)

      show_simple = simplePackageList my_flags . allPackagesInStack

  when (not (null broken) && not simple_output && verbosity /= Silent) $ do
     prog <- getProgramName
     putStrLn ("WARNING: there are broken packages.  Run '" ++ prog ++ " check' for more details.")

  if simple_output then show_simple stack else do

#if defined(mingw32_HOST_OS) || __GLASGOW_HASKELL__ < 611 || defined(BOOTSTRAPPING)
  mapM_ show_normal stack
#else
  let
     show_colour withF db =
         mconcat $ map (<#> termText "\n") $
             (termText (location db) :
                map (termText "   " <#>) (map pp_pkg (packages db)))
        where
                 pp_pkg p
                   | sourcePackageId p `elem` broken = withF Red  doc
                   | exposed p                       = doc
                   | otherwise                       = withF Blue doc
                   where doc | verbosity >= Verbose
                             = termText (printf "%s (%s)" pkg ipid)
                             | otherwise
                             = termText pkg
                          where
                          InstalledPackageId ipid = installedPackageId p
                          pkg = display (sourcePackageId p)

  is_tty <- hIsTerminalDevice stdout
  if not is_tty
     then mapM_ show_normal stack
     else do tty <- Terminfo.setupTermFromEnv
             case Terminfo.getCapability tty withForegroundColor of
                 Nothing -> mapM_ show_normal stack
                 Just w  -> runTermOutput tty $ mconcat $
                                                map (show_colour w) stack
#endif

simplePackageList :: [Flag] -> [InstalledPackageInfo] -> IO ()
simplePackageList my_flags pkgs = do
   let showPkg = if FlagNamesOnly `elem` my_flags then display . pkgName
                                                  else display
       strs = map showPkg $ sortBy compPkgIdVer $ map sourcePackageId pkgs
   when (not (null pkgs)) $
      hPutStrLn stdout $ concat $ intersperse " " strs

showPackageDot :: Verbosity -> [Flag] -> IO ()
showPackageDot verbosity myflags = do
  (_, _, flag_db_stack) <- 
      getPkgDatabases verbosity False True{-use cache-} myflags

  let all_pkgs = allPackagesInStack flag_db_stack
      ipix  = PackageIndex.fromList all_pkgs

  putStrLn "digraph {"
  let quote s = '"':s ++ "\""
  mapM_ putStrLn [ quote from ++ " -> " ++ quote to
                 | p <- all_pkgs,
                   let from = display (sourcePackageId p),
                   depid <- depends p,
                   Just dep <- [PackageIndex.lookupInstalledPackageId ipix depid],
                   let to = display (sourcePackageId dep)
                 ]
  putStrLn "}"

-- -----------------------------------------------------------------------------
-- Prints the highest (hidden or exposed) version of a package

latestPackage ::  Verbosity -> [Flag] -> PackageIdentifier -> IO ()
latestPackage verbosity my_flags pkgid = do
  (_, _, flag_db_stack) <- 
     getPkgDatabases verbosity False True{-use cache-} my_flags

  ps <- findPackages flag_db_stack (Id pkgid)
  show_pkg (sortBy compPkgIdVer (map sourcePackageId ps))
  where
    show_pkg [] = die "no matches"
    show_pkg pids = hPutStrLn stdout (display (last pids))

-- -----------------------------------------------------------------------------
-- Describe

describePackage :: Verbosity -> [Flag] -> PackageArg -> IO ()
describePackage verbosity my_flags pkgarg = do
  (_, _, flag_db_stack) <- 
      getPkgDatabases verbosity False True{-use cache-} my_flags
  ps <- findPackages flag_db_stack pkgarg
  doDump ps

dumpPackages :: Verbosity -> [Flag] -> IO ()
dumpPackages verbosity my_flags = do
  (_, _, flag_db_stack) <- 
     getPkgDatabases verbosity False True{-use cache-} my_flags
  doDump (allPackagesInStack flag_db_stack)

doDump :: [InstalledPackageInfo] -> IO ()
doDump = mapM_ putStrLn . intersperse "---" . map showInstalledPackageInfo

-- PackageId is can have globVersion for the version
findPackages :: PackageDBStack -> PackageArg -> IO [InstalledPackageInfo]
findPackages db_stack pkgarg
  = fmap (concatMap snd) $ findPackagesByDB db_stack pkgarg

findPackagesByDB :: PackageDBStack -> PackageArg
                 -> IO [(PackageDB, [InstalledPackageInfo])]
findPackagesByDB db_stack pkgarg
  = case [ (db, matched)
         | db <- db_stack,
           let matched = filter (pkgarg `matchesPkg`) (packages db),
           not (null matched) ] of
        [] -> die ("cannot find package " ++ pkg_msg pkgarg)
        ps -> return ps
  where
        pkg_msg (Id pkgid)           = display pkgid
        pkg_msg (Substring pkgpat _) = "matching " ++ pkgpat

matches :: PackageIdentifier -> PackageIdentifier -> Bool
pid `matches` pid'
  = (pkgName pid == pkgName pid')
    && (pkgVersion pid == pkgVersion pid' || not (realVersion pid))

realVersion :: PackageIdentifier -> Bool
realVersion pkgid = versionBranch (pkgVersion pkgid) /= []
  -- when versionBranch == [], this is a glob

matchesPkg :: PackageArg -> InstalledPackageInfo -> Bool
(Id pid)        `matchesPkg` pkg = pid `matches` sourcePackageId pkg
(Substring _ m) `matchesPkg` pkg = m (display (sourcePackageId pkg))

compPkgIdVer :: PackageIdentifier -> PackageIdentifier -> Ordering
compPkgIdVer p1 p2 = pkgVersion p1 `compare` pkgVersion p2

-- -----------------------------------------------------------------------------
-- Field

describeField :: Verbosity -> [Flag] -> PackageArg -> [String] -> IO ()
describeField verbosity my_flags pkgarg fields = do
  (_, _, flag_db_stack) <- 
      getPkgDatabases verbosity False True{-use cache-} my_flags
  fns <- toFields fields
  ps <- findPackages flag_db_stack pkgarg
  let top_dir = takeDirectory (location (last flag_db_stack))
  mapM_ (selectFields fns) (mungePackagePaths top_dir ps)
  where toFields [] = return []
        toFields (f:fs) = case toField f of
            Nothing -> die ("unknown field: " ++ f)
            Just fn -> do fns <- toFields fs
                          return (fn:fns)
        selectFields fns info = mapM_ (\fn->putStrLn (fn info)) fns

mungePackagePaths :: String -> [InstalledPackageInfo] -> [InstalledPackageInfo]
-- Replace the strings "$topdir" and "$httptopdir" at the beginning of a path
-- with the current topdir (obtained from the -B option).
mungePackagePaths top_dir ps = map munge_pkg ps
  where
  munge_pkg p = p{ importDirs        = munge_paths (importDirs p),
                   includeDirs       = munge_paths (includeDirs p),
                   libraryDirs       = munge_paths (libraryDirs p),
                   frameworkDirs     = munge_paths (frameworkDirs p),
                   haddockInterfaces = munge_paths (haddockInterfaces p),
                   haddockHTMLs      = munge_paths (haddockHTMLs p)
                 }

  munge_paths = map munge_path

  munge_path p
   | Just p' <- maybePrefixMatch "$topdir"     p =            top_dir ++ p'
   | Just p' <- maybePrefixMatch "$httptopdir" p = toHttpPath top_dir ++ p'
   | otherwise                               = p

  toHttpPath p = "file:///" ++ p

maybePrefixMatch :: String -> String -> Maybe String
maybePrefixMatch []    rest = Just rest
maybePrefixMatch (_:_) []   = Nothing
maybePrefixMatch (p:pat) (r:rest)
  | p == r    = maybePrefixMatch pat rest
  | otherwise = Nothing

toField :: String -> Maybe (InstalledPackageInfo -> String)
-- backwards compatibility:
toField "import_dirs"     = Just $ strList . importDirs
toField "source_dirs"     = Just $ strList . importDirs
toField "library_dirs"    = Just $ strList . libraryDirs
toField "hs_libraries"    = Just $ strList . hsLibraries
toField "extra_libraries" = Just $ strList . extraLibraries
toField "include_dirs"    = Just $ strList . includeDirs
toField "c_includes"      = Just $ strList . includes
toField "package_deps"    = Just $ strList . map display. depends
toField "extra_cc_opts"   = Just $ strList . ccOptions
toField "extra_ld_opts"   = Just $ strList . ldOptions
toField "framework_dirs"  = Just $ strList . frameworkDirs
toField "extra_frameworks"= Just $ strList . frameworks
toField s                 = showInstalledPackageInfoField s

strList :: [String] -> String
strList = show


-- -----------------------------------------------------------------------------
-- Check: Check consistency of installed packages

checkConsistency :: Verbosity -> [Flag] -> IO ()
checkConsistency verbosity my_flags = do
  (db_stack, _, _) <- getPkgDatabases verbosity True True{-use cache-} my_flags
         -- check behaves like modify for the purposes of deciding which
         -- databases to use, because ordering is important.

  let simple_output = FlagSimpleOutput `elem` my_flags

  let pkgs = allPackagesInStack db_stack

      checkPackage p = do
         (_,es) <- runValidate $ checkPackageConfig p db_stack False True
         if null es
            then return []
            else do
              when (not simple_output) $ do
                  reportError ("There are problems in package " ++ display (sourcePackageId p) ++ ":")
                  _ <- reportValidateErrors es "  " Nothing
                  return ()
              return [p]

  broken_pkgs <- concat `fmap` mapM checkPackage pkgs

  let filterOut pkgs1 pkgs2 = filter not_in pkgs2
        where not_in p = sourcePackageId p `notElem` all_ps
              all_ps = map sourcePackageId pkgs1

  let not_broken_pkgs = filterOut broken_pkgs pkgs
      (_, trans_broken_pkgs) = closure [] not_broken_pkgs
      all_broken_pkgs = broken_pkgs ++ trans_broken_pkgs

  when (not (null all_broken_pkgs)) $ do
    if simple_output
      then simplePackageList my_flags all_broken_pkgs
      else do
       reportError ("\nThe following packages are broken, either because they have a problem\n"++
                "listed above, or because they depend on a broken package.")
       mapM_ (hPutStrLn stderr . display . sourcePackageId) all_broken_pkgs

  when (not (null all_broken_pkgs)) $ exitWith (ExitFailure 1)


closure :: [InstalledPackageInfo] -> [InstalledPackageInfo]
        -> ([InstalledPackageInfo], [InstalledPackageInfo])
closure pkgs db_stack = go pkgs db_stack
 where
   go avail not_avail =
     case partition (depsAvailable avail) not_avail of
        ([],        not_avail') -> (avail, not_avail')
        (new_avail, not_avail') -> go (new_avail ++ avail) not_avail'

   depsAvailable :: [InstalledPackageInfo] -> InstalledPackageInfo
                 -> Bool
   depsAvailable pkgs_ok pkg = null dangling
        where dangling = filter (`notElem` pids) (depends pkg)
              pids = map installedPackageId pkgs_ok

        -- we want mutually recursive groups of package to show up
        -- as broken. (#1750)

brokenPackages :: [InstalledPackageInfo] -> [InstalledPackageInfo]
brokenPackages pkgs = snd (closure [] pkgs)

-- -----------------------------------------------------------------------------
-- Manipulating package.conf files

type InstalledPackageInfoString = InstalledPackageInfo_ String

convertPackageInfoOut :: InstalledPackageInfo -> InstalledPackageInfoString
convertPackageInfoOut
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map display e,
                 hiddenModules  = map display h }

convertPackageInfoIn :: InstalledPackageInfoString -> InstalledPackageInfo
convertPackageInfoIn
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map convert e,
                 hiddenModules  = map convert h }
    where convert = fromJust . simpleParse

writeNewConfig :: Verbosity -> FilePath -> [InstalledPackageInfo] -> IO ()
writeNewConfig verbosity filename ipis = do
  when (verbosity >= Normal) $
      hPutStr stdout "Writing new package config file... "
  createDirectoryIfMissing True $ takeDirectory filename
  let shown = concat $ intersperse ",\n "
                     $ map (show . convertPackageInfoOut) ipis
      fileContents = "[" ++ shown ++ "\n]"
  writeFileAtomic filename fileContents
    `catch` \e ->
      if isPermissionError e
      then die (filename ++ ": you don't have permission to modify this file")
      else ioError e
  when (verbosity >= Normal) $
      hPutStrLn stdout "done."

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

type ValidateError = (Force,String)

newtype Validate a = V { runValidate :: IO (a, [ValidateError]) }

instance Monad Validate where
   return a = V $ return (a, [])
   m >>= k = V $ do
      (a, es) <- runValidate m
      (b, es') <- runValidate (k a)
      return (b,es++es')

verror :: Force -> String -> Validate ()
verror f s = V (return ((),[(f,s)]))

liftIO :: IO a -> Validate a
liftIO k = V (k >>= \a -> return (a,[]))

-- returns False if we should die
reportValidateErrors :: [ValidateError] -> String -> Maybe Force -> IO Bool
reportValidateErrors es prefix mb_force = do
  oks <- mapM report es
  return (and oks)
  where
    report (f,s)
      | Just force <- mb_force
      = if (force >= f)
           then do reportError (prefix ++ s ++ " (ignoring)")
                   return True
           else if f < CannotForce
                   then do reportError (prefix ++ s ++ " (use --force to override)")
                           return False
                   else do reportError err
                           return False
      | otherwise = do reportError err
                       return False
      where
             err = prefix ++ s

validatePackageConfig :: InstalledPackageInfo
                      -> PackageDBStack
                      -> Bool   -- auto-ghc-libs
                      -> Bool   -- update, or check
                      -> Force
                      -> IO ()
validatePackageConfig pkg db_stack auto_ghci_libs update force = do
  (_,es) <- runValidate $ checkPackageConfig pkg db_stack auto_ghci_libs update
  ok <- reportValidateErrors es (display (sourcePackageId pkg) ++ ": ") (Just force)
  when (not ok) $ exitWith (ExitFailure 1)

checkPackageConfig :: InstalledPackageInfo
                      -> PackageDBStack
                      -> Bool   -- auto-ghc-libs
                      -> Bool   -- update, or check
                      -> Validate ()
checkPackageConfig pkg db_stack auto_ghci_libs update = do
  checkInstalledPackageId pkg db_stack update
  checkPackageId pkg
  checkDuplicates db_stack pkg update
  mapM_ (checkDep db_stack) (depends pkg)
  checkDuplicateDepends (depends pkg)
  mapM_ (checkDir "import-dirs") (importDirs pkg)
  mapM_ (checkDir "library-dirs") (libraryDirs pkg)
  mapM_ (checkDir "include-dirs") (includeDirs pkg)
  checkModules pkg
  mapM_ (checkHSLib (libraryDirs pkg) auto_ghci_libs) (hsLibraries pkg)
  -- ToDo: check these somehow?
  --    extra_libraries :: [String],
  --    c_includes      :: [String],

checkInstalledPackageId :: InstalledPackageInfo -> PackageDBStack -> Bool 
                        -> Validate ()
checkInstalledPackageId ipi db_stack update = do
  let ipid@(InstalledPackageId str) = installedPackageId ipi
  when (null str) $ verror CannotForce "missing id field"
  let dups = [ p | p <- allPackagesInStack db_stack, 
                   installedPackageId p == ipid ]
  when (not update && not (null dups)) $
    verror CannotForce $
        "package(s) with this id already exist: " ++ 
         unwords (map (display.packageId) dups)

-- When the package name and version are put together, sometimes we can
-- end up with a package id that cannot be parsed.  This will lead to
-- difficulties when the user wants to refer to the package later, so
-- we check that the package id can be parsed properly here.
checkPackageId :: InstalledPackageInfo -> Validate ()
checkPackageId ipi =
  let str = display (sourcePackageId ipi) in
  case [ x :: PackageIdentifier | (x,ys) <- readP_to_S parse str, all isSpace ys ] of
    [_] -> return ()
    []  -> verror CannotForce ("invalid package identifier: " ++ str)
    _   -> verror CannotForce ("ambiguous package identifier: " ++ str)

checkDuplicates :: PackageDBStack -> InstalledPackageInfo -> Bool -> Validate ()
checkDuplicates db_stack pkg update = do
  let
        pkgid = sourcePackageId pkg
        pkgs  = packages (head db_stack)
  --
  -- Check whether this package id already exists in this DB
  --
  when (not update && (pkgid `elem` map sourcePackageId pkgs)) $
       verror CannotForce $
          "package " ++ display pkgid ++ " is already installed"

  let
        uncasep = map toLower . display
        dups = filter ((== uncasep pkgid) . uncasep) (map sourcePackageId pkgs)

  when (not update && not (null dups)) $ verror ForceAll $
        "Package names may be treated case-insensitively in the future.\n"++
        "Package " ++ display pkgid ++
        " overlaps with: " ++ unwords (map display dups)


checkDir :: String -> String -> Validate ()
checkDir thisfield d
 | "$topdir"     `isPrefixOf` d = return ()
 | "$httptopdir" `isPrefixOf` d = return ()
        -- can't check these, because we don't know what $(http)topdir is
 | otherwise = do
   there <- liftIO $ doesDirectoryExist d
   when (not there) $
       verror ForceFiles (thisfield ++ ": " ++ d ++ " doesn't exist or isn't a directory")

checkDep :: PackageDBStack -> InstalledPackageId -> Validate ()
checkDep db_stack pkgid
  | pkgid `elem` pkgids = return ()
  | otherwise = verror ForceAll ("dependency \"" ++ display pkgid
                                 ++ "\" doesn't exist")
  where
        all_pkgs = allPackagesInStack db_stack
        pkgids = map installedPackageId all_pkgs

checkDuplicateDepends :: [InstalledPackageId] -> Validate ()
checkDuplicateDepends deps
  | null dups = return ()
  | otherwise = verror ForceAll ("package has duplicate dependencies: " ++
                                     unwords (map display dups))
  where
       dups = [ p | (p:_:_) <- group (sort deps) ]

checkHSLib :: [String] -> Bool -> String -> Validate ()
checkHSLib dirs auto_ghci_libs lib = do
  let batch_lib_file = "lib" ++ lib ++ ".a"
  m <- liftIO $ doesFileExistOnPath batch_lib_file dirs
  case m of
    Nothing -> verror ForceFiles ("cannot find " ++ batch_lib_file ++
                                   " on library path")
    Just dir -> liftIO $ checkGHCiLib dirs dir batch_lib_file lib auto_ghci_libs

doesFileExistOnPath :: String -> [FilePath] -> IO (Maybe FilePath)
doesFileExistOnPath file path = go path
  where go []     = return Nothing
        go (p:ps) = do b <- doesFileExistIn file p
                       if b then return (Just p) else go ps

doesFileExistIn :: String -> String -> IO Bool
doesFileExistIn lib d
 | "$topdir"     `isPrefixOf` d = return True
 | "$httptopdir" `isPrefixOf` d = return True
 | otherwise                = doesFileExist (d </> lib)

checkModules :: InstalledPackageInfo -> Validate ()
checkModules pkg = do
  mapM_ findModule (exposedModules pkg ++ hiddenModules pkg)
  where
    findModule modl = do
      -- there's no .hi file for GHC.Prim
      if modl == fromString "GHC.Prim" then return () else do
      let file = toFilePath modl <.> "hi"
      m <- liftIO $ doesFileExistOnPath file (importDirs pkg)
      when (isNothing m) $
         verror ForceFiles ("file " ++ file ++ " is missing")

checkGHCiLib :: [String] -> String -> String -> String -> Bool -> IO ()
checkGHCiLib dirs batch_lib_dir batch_lib_file lib auto_build
  | auto_build = autoBuildGHCiLib batch_lib_dir batch_lib_file ghci_lib_file
  | otherwise  = do
      m <- doesFileExistOnPath ghci_lib_file dirs
      when (isNothing m && ghci_lib_file /= "HSrts.o") $
        hPutStrLn stderr ("warning: can't find GHCi lib " ++ ghci_lib_file)
 where
    ghci_lib_file = lib <.> "o"

-- automatically build the GHCi version of a batch lib,
-- using ld --whole-archive.

autoBuildGHCiLib :: String -> String -> String -> IO ()
autoBuildGHCiLib dir batch_file ghci_file = do
  let ghci_lib_file  = dir ++ '/':ghci_file
      batch_lib_file = dir ++ '/':batch_file
  hPutStr stderr ("building GHCi library " ++ ghci_lib_file ++ "...")
#if defined(darwin_HOST_OS)
  r <- rawSystem "ld" ["-r","-x","-o",ghci_lib_file,"-all_load",batch_lib_file]
#elif defined(mingw32_HOST_OS)
  execDir <- getLibDir
  r <- rawSystem (maybe "" (++"/gcc-lib/") execDir++"ld") ["-r","-x","-o",ghci_lib_file,"--whole-archive",batch_lib_file]
#else
  r <- rawSystem "ld" ["-r","-x","-o",ghci_lib_file,"--whole-archive",batch_lib_file]
#endif
  when (r /= ExitSuccess) $ exitWith r
  hPutStrLn stderr (" done.")

-- -----------------------------------------------------------------------------
-- Searching for modules

#if not_yet

findModules :: [FilePath] -> IO [String]
findModules paths =
  mms <- mapM searchDir paths
  return (concat mms)

searchDir path prefix = do
  fs <- getDirectoryEntries path `catch` \_ -> return []
  searchEntries path prefix fs

searchEntries path prefix [] = return []
searchEntries path prefix (f:fs)
  | looks_like_a_module  =  do
        ms <- searchEntries path prefix fs
        return (prefix `joinModule` f : ms)
  | looks_like_a_component  =  do
        ms <- searchDir (path </> f) (prefix `joinModule` f)
        ms' <- searchEntries path prefix fs
        return (ms ++ ms')
  | otherwise
        searchEntries path prefix fs

  where
        (base,suffix) = splitFileExt f
        looks_like_a_module =
                suffix `elem` haskell_suffixes &&
                all okInModuleName base
        looks_like_a_component =
                null suffix && all okInModuleName base

okInModuleName c

#endif

-- ---------------------------------------------------------------------------
-- expanding environment variables in the package configuration

expandEnvVars :: String -> Force -> IO String
expandEnvVars str0 force = go str0 ""
 where
   go "" acc = return $! reverse acc
   go ('$':'{':str) acc | (var, '}':rest) <- break close str
        = do value <- lookupEnvVar var
             go rest (reverse value ++ acc)
        where close c = c == '}' || c == '\n' -- don't span newlines
   go (c:str) acc
        = go str (c:acc)

   lookupEnvVar :: String -> IO String
   lookupEnvVar nm =
        catch (System.Environment.getEnv nm)
           (\ _ -> do dieOrForceAll force ("Unable to expand variable " ++
                                        show nm)
                      return "")

-----------------------------------------------------------------------------

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die = dieWith 1

dieWith :: Int -> String -> IO a
dieWith ec s = do
  hFlush stdout
  prog <- getProgramName
  hPutStrLn stderr (prog ++ ": " ++ s)
  exitWith (ExitFailure ec)

dieOrForceAll :: Force -> String -> IO ()
dieOrForceAll ForceAll s = ignoreError s
dieOrForceAll _other s   = dieForcible s

ignoreError :: String -> IO ()
ignoreError s = reportError (s ++ " (ignoring)")

reportError :: String -> IO ()
reportError s = do hFlush stdout; hPutStrLn stderr s

dieForcible :: String -> IO ()
dieForcible s = die (s ++ " (use --force to override)")

my_head :: String -> [a] -> a
my_head s []      = error s
my_head _ (x : _) = x

-----------------------------------------
-- Cut and pasted from ghc/compiler/main/SysTools

#if defined(mingw32_HOST_OS)
subst :: Char -> Char -> String -> String
subst a b ls = map (\ x -> if x == a then b else x) ls

unDosifyPath :: FilePath -> FilePath
unDosifyPath xs = subst '\\' '/' xs

getLibDir :: IO (Maybe String)
getLibDir = fmap (fmap (</> "lib")) $ getExecDir "/bin/lhc-pkg.exe"

-- (getExecDir cmd) returns the directory in which the current
--                  executable, which should be called 'cmd', is running
-- So if the full path is /a/b/c/d/e, and you pass "d/e" as cmd,
-- you'll get "/a/b/c" back as the result
getExecDir :: String -> IO (Maybe String)
getExecDir cmd =
    getExecPath >>= maybe (return Nothing) removeCmdSuffix
    where initN n = reverse . drop n . reverse
          removeCmdSuffix = return . Just . initN (length cmd) . unDosifyPath

getExecPath :: IO (Maybe String)
getExecPath =
     allocaArray len $ \buf -> do
         ret <- getModuleFileName nullPtr buf len
         if ret == 0 then return Nothing
	             else liftM Just $ peekCString buf
    where len = 2048 -- Plenty, PATH_MAX is 512 under Win32.

foreign import stdcall unsafe "GetModuleFileNameA"
    getModuleFileName :: Ptr () -> CString -> Int -> IO Int32

#else
getLibDir :: IO (Maybe String)
getLibDir = return Nothing
#endif

-----------------------------------------
-- Adapted from ghc/compiler/utils/Panic

installSignalHandlers :: IO ()
installSignalHandlers = do
  threadid <- myThreadId
  let
      interrupt = Exception.throwTo threadid
                                    (Exception.ErrorCall "interrupted")
  --
#if !defined(mingw32_HOST_OS)
  _ <- installHandler sigQUIT (Catch interrupt) Nothing
  _ <- installHandler sigINT  (Catch interrupt) Nothing
  return ()
#elif __GLASGOW_HASKELL__ >= 603
  -- GHC 6.3+ has support for console events on Windows
  -- NOTE: running GHCi under a bash shell for some reason requires
  -- you to press Ctrl-Break rather than Ctrl-C to provoke
  -- an interrupt.  Ctrl-C is getting blocked somewhere, I don't know
  -- why --SDM 17/12/2004
  let sig_handler ControlC = interrupt
      sig_handler Break    = interrupt
      sig_handler _        = return ()

  _ <- installHandler (Catch sig_handler)
  return ()
#else
  return () -- nothing
#endif

#if __GLASGOW_HASKELL__ <= 604
isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
#endif

#if mingw32_HOST_OS || mingw32_TARGET_OS
throwIOIO :: Exception.IOException -> IO a
throwIOIO = Exception.throwIO

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch
#endif

catchError :: IO a -> (String -> IO a) -> IO a
catchError io handler = io `Exception.catch` handler'
    where handler' (Exception.ErrorCall err) = handler err


-- copied from Cabal's Distribution.Simple.Utils, except that we want
-- to use text files here, rather than binary files.
writeFileAtomic :: FilePath -> String -> IO ()
writeFileAtomic targetFile content = do
  (newFile, newHandle) <- openNewFile targetDir template
  do  hPutStr newHandle content
      hClose newHandle
#if mingw32_HOST_OS || mingw32_TARGET_OS
      renameFile newFile targetFile
        -- If the targetFile exists then renameFile will fail
        `catchIO` \err -> do
          exists <- doesFileExist targetFile
          if exists
            then do removeFile targetFile
                    -- Big fat hairy race condition
                    renameFile newFile targetFile
                    -- If the removeFile succeeds and the renameFile fails
                    -- then we've lost the atomic property.
            else throwIOIO err
#else
      renameFile newFile targetFile
#endif
   `Exception.onException` do hClose newHandle
                              removeFile newFile
  where
    template = targetName <.> "tmp"
    targetDir | null targetDir_ = "."
              | otherwise       = targetDir_
    --TODO: remove this when takeDirectory/splitFileName is fixed
    --      to always return a valid dir
    (targetDir_,targetName) = splitFileName targetFile

-- Ugh, this is a copy/paste of code from the base library, but
-- if uses 666 rather than 600 for the permissions.
openNewFile :: FilePath -> String -> IO (FilePath, Handle)
openNewFile dir template = do
  pid <- c_getpid
  findTempName pid
  where
    -- We split off the last extension, so we can use .foo.ext files
    -- for temporary files (hidden on Unix OSes). Unfortunately we're
    -- below filepath in the hierarchy here.
    (prefix,suffix) =
       case break (== '.') $ reverse template of
         -- First case: template contains no '.'s. Just re-reverse it.
         (rev_suffix, "")       -> (reverse rev_suffix, "")
         -- Second case: template contains at least one '.'. Strip the
         -- dot from the prefix and prepend it to the suffix (if we don't
         -- do this, the unique number will get added after the '.' and
         -- thus be part of the extension, which is wrong.)
         (rev_suffix, '.':rest) -> (reverse rest, '.':reverse rev_suffix)
         -- Otherwise, something is wrong, because (break (== '.')) should
         -- always return a pair with either the empty string or a string
         -- beginning with '.' as the second component.
         _                      -> error "bug in System.IO.openTempFile"

    oflags = rw_flags .|. o_EXCL

#if __GLASGOW_HASKELL__ < 611
    withFilePath = withCString
#endif

    findTempName x = do
      fd <- withFilePath filepath $ \ f ->
              c_open f oflags 0o666
      if fd < 0
       then do
         errno <- getErrno
         if errno == eEXIST
           then findTempName (x+1)
           else ioError (errnoToIOError "openNewBinaryFile" errno Nothing (Just dir))
       else do
         -- XXX We want to tell fdToHandle what the filepath is,
         -- as any exceptions etc will only be able to report the
         -- fd currently
         h <-
#if __GLASGOW_HASKELL__ >= 609
              fdToHandle fd
#else
              fdToHandle (fromIntegral fd)
#endif
              `Exception.onException` c_close fd
         return (filepath, h)
      where
        filename        = prefix ++ show x ++ suffix
        filepath        = dir `combine` filename

-- XXX Copied from GHC.Handle
std_flags, output_flags, rw_flags :: CInt
std_flags    = o_NONBLOCK   .|. o_NOCTTY
output_flags = std_flags    .|. o_CREAT
rw_flags     = output_flags .|. o_RDWR

-- | The function splits the given string to substrings
-- using 'isSearchPathSeparator'.
parseSearchPath :: String -> [FilePath]
parseSearchPath path = split path
  where
    split :: String -> [String]
    split s =
      case rest' of
        []     -> [chunk]
        _:rest -> chunk : split rest
      where
        chunk =
          case chunk' of
#ifdef mingw32_HOST_OS
            ('\"':xs@(_:_)) | last xs == '\"' -> init xs
#endif
            _                                 -> chunk'

        (chunk', rest') = break isSearchPathSeparator s
