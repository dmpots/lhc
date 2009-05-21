import System.Cmd (system)
import System.FilePath ((</>))
import Control.Monad (when, unless)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.InstallDirs (CopyDest(..))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs, InstallDirs(..))

lhclibdir = "lib"
libsToBuild = map (lhclibdir </>) [ "ghc-prim", "integer-native", "base" ]


main = defaultMainWithHooks simpleUserHooks { postInst = myPostInst }
  where myPostInst _ _ pkgdesc buildinfo = do
          let dirs   = absoluteInstallDirs pkgdesc buildinfo NoCopyDest
              lhc    = bindir dirs </> "lhc"
              lhcpkg = bindir dirs </> "lhc-pkg"
              confargs = unwords [ "--lhc", "--with-lhc="++lhc, "--with-lhc-pkg="++lhcpkg
                                 , "--prefix="++show (prefix (installDirTemplates buildinfo))]
              lpkgdesc = localPkgDescr buildinfo
              exes     = executables lpkgdesc
              sanity   = any (\(Executable s _ _) -> s == "lhc") exes
          unless sanity $ fail "No lhc executale found - this probably shouldn't happen"
          let lhcexe   = head $ filter (\(Executable s _ _) -> s == "lhc") exes
              binfo    = buildInfo lhcexe
              customF  = customFieldsBI binfo
          when (withLibs customF) $ installLhcPkgs confargs libsToBuild
        withLibs = any $ \(x,y) -> x == "x-build-libs" && y == "True"
        installLhcPkgs cf  = mapM_ (installLhcPkg cf)
        installLhcPkg cf n = do
            putStrLn $ "\n[installing "++n++" package for lhc]\n"
            let x = unwords ["cd",n
                            ,"&&","runghc Setup configure",cf
                            ,"&&","runghc Setup build"
                            ,"&&","sudo runghc Setup copy"
                            ,"&&","runghc Setup register"]
            putStrLn $ x
            system x
            return ()
