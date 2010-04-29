import System.Cmd (system)
import System.Exit(ExitCode(..), exitWith)
import System.IO
import System.Process (readProcess)
import System.FilePath
import Control.Monad (when, unless, liftM)
import System.Directory
import System.Info as SysVer
import Data.Version
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.InstallDirs (CopyDest(..))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs, InstallDirs(..))

lhclibdir = "lib"
libsToBuild = map (lhclibdir </>) [ "ghc-prim", "integer-simple", "base" ]

main = defaultMainWithHooks simpleUserHooks {   postInst  = myPostInst
                                              , postClean = cleanLibs }
  where myPostInst _ _ pkgdesc buildinfo = do
          let dirs   = absoluteInstallDirs pkgdesc buildinfo NoCopyDest
              pkgVer = pkgVersion (package pkgdesc)
              lhc    = bindir dirs </> "lhc"
              lhce   = bindir dirs </> "lhc-exe"
              lhcpkg = bindir dirs </> "lhc-pkg"
              lhcpkge= bindir dirs </> "lhc-pkg-exe"
              lpkgdesc = localPkgDescr buildinfo
              exes     = executables lpkgdesc
              sanity   = any (\(Executable s _ _) -> s == "lhc-exe") exes
          unless sanity $ fail "No lhc executale found - this probably shouldn't happen"
          let lhcexe   = head $ filter (\(Executable s _ _) -> s == "lhc-exe") exes
              binfo    = buildInfo lhcexe
              customF  = customFieldsBI binfo
          -- initial setup
          udir' <- getAppUserDataDirectory "lhc"
          -- NOTE - THIS MUST BE KEPT IN SYNC WITH
          -- lhc-pkg in lhc-pkg/Main.hs!!!
          let udir =  udir' </> (SysVer.arch ++ "-" ++ SysVer.os ++  "-" ++ (showVersion pkgVer))
          let ldir =  (libdir dirs)
--            pkgconf = udir </> "package" <.> "conf"
--          b <- doesFileExist pkgconf
--          unless b $ do
--            putStr "Creating initial package.conf file..."
--            createDirectoryIfMissing True udir
--            writeFile (udir </> "package.conf") "[]\n"
--            putStrLn "Done"

              global_pkg_conf = ldir </> "package.conf.d"
              global_packages = ldir </> "packages"
              local_pkg_conf  = udir </> "package.conf.d"
              local_packages  = udir </> "packages"
              create conf = lhcpkge ++ " init " ++ conf
              mkdir  dir  = "mkdir " ++ dir
          b <- doesDirectoryExist global_pkg_conf
          unless b $ do
            putStr "Creating global package conf..."
            runCommand (create global_pkg_conf) "Done"

          b <- doesDirectoryExist global_packages
          unless b $ do
            putStr "Creating global packages directory..."
            runCommand (mkdir global_packages) "Done"

          b <- doesDirectoryExist local_pkg_conf
          unless b $ do
            putStr "Creating local package conf..."
            runCommand (create local_pkg_conf) "Done"

          b <- doesDirectoryExist local_packages
          unless b $ do
            putStr "Creating local packages directory..."
            runCommand (mkdir local_packages) "Done"

          -- Create lhc-pkg executable
          h <- openFile lhcpkg WriteMode 
          hPutStrLn h $ "#!/bin/sh"
          hPutStrLn h $ "PKGCONF=\""++global_pkg_conf++"\""
          hPutStrLn h $ "executablename=\""++lhcpkge++"\""
          hPutStrLn h $ "exec \"$executablename\" --global-conf \"$PKGCONF\" ${1+\"$@\"}"
          hClose h 
          setExecutable lhcpkg

          -- Create lhc executable
          h <- openFile lhc WriteMode 
          hPutStrLn h $ "#!/bin/sh"
          hPutStrLn h $ "topdir=\""++ldir++"\""
          hPutStrLn h $ "executablename=\""++lhce++"\""
          hPutStrLn h $ "exec \"$executablename\" -B\"$topdir\" ${1+\"$@\"}"
          hClose h 
          setExecutable lhc


          -- copy over extra-gcc-opts and unlit from
          -- ghc's libdir
          -- NOTE FIXME: this assumes that the 'ghc' executable
          -- points to the same one you compiled LHC against; although,
          -- the compile options would probably roughly stay the same anyway
          ghcLibdir <- liftM (unwords . words) $ readProcess "ghc" ["--print-libdir"] []
          let unlit        = ghcLibdir </> "unlit"
              extragccopts = ghcLibdir </> "extra-gcc-opts"
          putStr "Copying unlit and extra-gcc-opts..."
          system $ "cp "++unlit++" "++(ldir </> "unlit")
          system $ "cp "++extragccopts++" "++(ldir </> "extra-gcc-opts")
          putStrLn "Done"
          -- build libraries if -fwith-libs is passed
          when (withLibs customF) $ do
            let confargs = unwords [ "--lhc", "--with-lhc="++lhc, "--with-lhc-pkg="++lhcpkg
                                   , "--prefix="++show (prefix (installDirTemplates buildinfo))
                                   , "--extra-include-dirs="++(ghcLibdir</>"include") ]
            putStrLn "building libraries..."
            installLhcPkgs confargs libsToBuild

        withLibs = any $ \(x,y) -> x == "x-build-libs" && y == "True"
        installLhcPkgs cf  = mapM_ (installLhcPkg cf)
        installLhcPkg cf n = do
            putStrLn $ "\n[installing "++n++" package for lhc]\n"
            let x = unwords ["cd",n
                            ,"&&"," ghc --make Setup "
                            ,"&&"," ./Setup configure ",cf
                            ,"&&"," ./Setup build "
                            ,"&&"," ./Setup copy "
                            ,"&&"," ./Setup register "]
            putStrLn $ x
            runCommand x "\nDone"

--cleanLibs :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
cleanLibs _ _ _ _ = mapM_ (\n -> runClean n) libsToBuild
  where runClean n = do {
      putStrLn ("Cleaning " ++ n)
    ; runCommand ("cd "++n++" && cabal clean") ("Cleaned "++n)
  }


runCommand :: String -> String -> IO ()
runCommand cmd successMsg = do
  retVal <- system cmd
  case retVal of
    ExitSuccess -> do 
      putStrLn successMsg
      return ()
    ExitFailure c -> do
        putStrLn ("\nFailed with exit code: " ++ (show c))
        exitWith retVal


setExecutable :: FilePath -> IO ()
setExecutable file = do
  p <- getPermissions file
  setPermissions file p {executable = True}

