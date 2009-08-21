module LhcMain (tryMain) where

import System.Directory
import System.FilePath
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO
import System.Exit
import qualified Data.Map as Map
import Data.Binary
import Data.Maybe
import Control.Monad

import CompactString
import qualified Language.Core as Core
import Grin.SimpleCore
import Grin.FromCore
import Grin.Pretty
import qualified Grin.SimpleCore.DeadCode as Simple
import qualified Grin.Eval.Compile as Compile
import qualified Grin.Optimize.Simple as Simple
import qualified Grin.DeadCode as DeadCode

--import Grin.Rename
import qualified Grin.HPT as HPT
import qualified Grin.Lowering.Apply as Apply

import qualified Grin.Stage2.FromStage1 as Stage2
import qualified Grin.Stage2.Pretty as Stage2
import qualified Grin.Stage2.Optimize.Simple as Stage2.Simple
--import qualified Grin.Stage2.Backend.LLVM as Backend.LLVM
import qualified Grin.Stage2.Backend.C as Backend.C
import qualified Grin.Stage2.DeadCode  as Stage2

-- TODO: We need proper command line parsing.
tryMain :: IO ()
tryMain = do args <- getArgs
             case args of
               ("install":files)      -> mapM_ installCoreFile files >> exitWith ExitSuccess
               ("compile":files)  -> build Compile files >> exitWith ExitSuccess
               ("benchmark":file:[]) -> build Benchmark [file] >> exitWith ExitSuccess
               ("execute":file:args)  -> execute file args >> exitWith ExitSuccess
               _ -> return ()


installCoreFile :: FilePath -> IO ()
installCoreFile path
    = do inp <- L.readFile path
         hPutStr stderr $ "Parsing " ++ path ++ "..."
         hFlush stdout
         case Core.parseModule "file" inp of
           Left errs -> hPutStrLn stderr "errors: " >> print errs
           Right mod  -> do hPutStrLn stderr " done"
                            dataDir <- getAppUserDataDirectory "lhc"
                            let smod = coreToSimpleCore mod
                            createDirectoryIfMissing False (dataDir </> modulePackage smod)
                            encodeFile (dataDir </> modulePackage smod </> moduleName smod) smod

data Action = Compile | Benchmark

build :: Action -> [FilePath] -> IO ()
build action files@(file:_)
    = do mods <- mapM parseCore files
         libs <- loadAllLibraries
         let primModule = SimpleModule { modulePackage = "ghczmprim"
                                       , moduleName    = "GHCziPrim"
                                       , moduleTypes   = [SimpleType (fromString "ghc-prim:GHC.Prim.(# #)") 1
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,#)") 2
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,#)") 3
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,#)") 4
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,#)") 5
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,#)") 6
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,#)") 7
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,#)") 8]
                                       , moduleDefs    = [] }
         let allModules = Map.insert (modulePackage primModule, moduleName primModule) primModule $
                          foldr (\mod -> Map.insert (modulePackage mod, moduleName mod) mod) libs mods
                          -- libs
             (tdefs, defs) = Simple.removeDeadCode [("main","Main")]  ["main::Main.main"] allModules
             grin = coreToGrin tdefs defs
             opt = iterate Simple.optimize grin !! 2
             applyLowered = Apply.lower opt
             (iterations, hpt) = HPT.analyze applyLowered
             (evalLowered, hpt') = HPT.lower hpt applyLowered
             opt' = iterate Simple.optimize evalLowered !! 2
             trimmed = DeadCode.removeDeadCode opt'
             out = trimmed
         let stage2_raw = Stage2.convert hpt' out
             stage2_opt = iterate Stage2.Simple.optimize stage2_raw !! 2
             stage2_trim = Stage2.trimDeadCode stage2_opt
             stage2_opt' = iterate Stage2.Simple.optimize stage2_trim !! 2
             stage2_out = stage2_opt'
             --llvmModule = Backend.LLVM.fromGrin stage2_out
         case action of
           Benchmark -> do let target = replaceExtension file "lhc"
                           Backend.C.compileFastCode stage2_out (dropExtension target)
           Compile   -> do let target = replaceExtension file "lhc"
                           outputGrin target "_raw" grin
                           outputGrin target "_simple" opt
                           outputGrin target "_apply" applyLowered
                           outputGrin target "_eval" evalLowered
                           writeFile (replaceExtension file "hpt") (show hpt')
                           outputGrin target "_trimmed" trimmed
                           outputGrin target "" out
                           outputGrin2 target "_raw" stage2_raw
                           outputGrin2 target "_opt" stage2_opt
                           outputGrin2 target "_trimmed" stage2_trim
                           outputGrin2 target "" stage2_out
                           --writeFile (replaceExtension target ".ll") (show $ Backend.LLVM.ppModule llvmModule)]
                           Backend.C.compile stage2_out (dropExtension target)
                           --Stage2.calcLiveNodes stage2_out

                           putStrLn $ "Fixpoint found in " ++ show iterations ++ " iterations."

                           --lhc <- findExecutable "lhc"
                           --L.writeFile target $ L.unlines [ L.pack $ "#!" ++ fromMaybe "/usr/bin/env lhc" lhc ++ " execute"
                           --                               , encode out ]
                           --perm <- getPermissions target
                           --setPermissions target perm{executable = True}

outputGrin file variant grin
    = do let outputFile = replaceExtension file ("grin"++variant)
         writeFile outputFile (show $ ppGrin grin)
         return ()

outputGrin2 file variant grin
    = do let outputFile = replaceExtension file ("grin2"++variant)
         writeFile outputFile (show $ Stage2.ppGrin grin)
         return ()

execute :: FilePath -> [String] -> IO ()
execute path args
    = do inp <- L.readFile path
         let grin = decode (dropHashes inp)
         Compile.runGrin grin args
         return ()
    where dropHashes inp | L.pack "#" `L.isPrefixOf` inp = L.unlines (drop 1 (L.lines inp))
                         | otherwise = inp


loadAllLibraries :: IO (Map.Map ModuleIdent SimpleModule)
loadAllLibraries
    = do dataDir <- getAppUserDataDirectory "lhc"
         packages <- getDirectoryContents dataDir
         smods <- forM (filter (`notElem` [".",".."]) packages) $ \package ->
                  do modules <- getDirectoryContents (dataDir </> package)
                     forM (filter (`notElem` [".",".."]) modules) $ \mod ->
                       do smod <- decodeFile (dataDir </> package </> mod)
                          return ((package,mod),smod)
         return $ Map.fromList [ (ident, mod) | (ident,mod) <- concat smods ]


parseCore :: FilePath -> IO SimpleModule
parseCore path
    = do inp <- L.readFile path
         case Core.parseModule path inp of
           Left errs -> error (show errs)
           Right mod -> do --putStrLn $ "parsing done: " ++ path
                           return (coreToSimpleCore mod)
