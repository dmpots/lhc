import System.Cmd
import System.FilePath
import Distribution.Simple
import Distribution.Simple.InstallDirs (CopyDest(..))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs, InstallDirs(..))

main = defaultMainWithHooks simpleUserHooks { postInst = myPostInst }
  where myPostInst _ _ pkgdesc buildinfo = do 
          let dirs  = absoluteInstallDirs pkgdesc buildinfo NoCopyDest
              datad = datadir dirs
              lhc   = bindir dirs </> "lhc"
              confargs = "--lhc --with-lhc="++lhc++" --prefix="++datad
          installLhcPkgs confargs ["base"]
        installLhcPkgs cf  = mapM_ (installLhcPkg cf)
        installLhcPkg cf n = do putStrLn $ "\n[installing "++n++" package for lhc]\n"
                                let dir = "lib" </> n
                                system $ concat ["cd ",dir," && runghc Setup configure ",cf," && runghc Setup build && runghc Setup install"]
                                return ()
