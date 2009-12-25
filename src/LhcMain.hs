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
import Data.Time; import Text.Printf

import CompactString
import qualified Language.Core as Core
import Grin.SimpleCore
import Grin.FromCore
import Grin.Pretty
import qualified Grin.SimpleCore.DeadCode as Simple
import qualified Grin.Optimize.Simple as Simple
import qualified Grin.Optimize.Case as Case
import qualified Grin.DeadCode as DeadCode
import qualified Grin.PreciseDeadCode as DeadCode
import qualified Grin.Optimize.Inline as Inline

--import Grin.Rename
import qualified Grin.HPT as HPT
import qualified Grin.Lowering.Apply as Apply

import qualified Grin.Stage2.FromStage1 as Stage2
import qualified Grin.Stage2.Pretty as Stage2
import qualified Grin.Stage2.Optimize.Simple as Stage2.Simple
import qualified Grin.Stage2.Optimize.Case as Stage2.Case
import qualified Grin.Stage2.Backend.LLVM as Backend.LLVM
import qualified Grin.Stage2.Backend.C as Backend.C
import qualified Grin.Stage2.DeadCode  as Stage2
import qualified Grin.Stage2.Rename    as Stage2

import Manager

--import Tick

-- TODO: We need proper command line parsing.
tryMain :: IO ()
tryMain = do args <- getArgs
             case args of
               ("install":files)      -> mapM_ installCoreFile files >> exitWith ExitSuccess
               ("compile":files)  -> build Compile files >> exitWith ExitSuccess
               ("benchmark":files) -> build Benchmark files >> exitWith ExitSuccess
               ("llvm":files) -> build LLVM files >> exitWith ExitSuccess
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
                            let packagesDir = dataDir </> "packages"
                            let smod = coreToSimpleCore mod
                            createDirectoryIfMissing False (packagesDir </> modulePackage smod)
                            encodeFile (packagesDir </> modulePackage smod </> moduleName smod) smod

data Action = Compile | Benchmark | LLVM

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
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,#)") 8
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,,#)") 9
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,,,#)") 10
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,,,,#)") 11
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,,,,,#)") 12
                                                         ]
                                       , moduleEnums   = []

                                       , moduleDefs    = [] }
         let allModules = Map.insert (modulePackage primModule, moduleName primModule) primModule $
                          foldr (\mod -> Map.insert (modulePackage mod, moduleName mod) mod) libs mods
                          -- libs
             (tdefs, enums, defs) = Simple.removeDeadCode [("main","Main")]  ["main::Main.main"] allModules
             grin = coreToGrin tdefs enums defs
         let target = replaceExtension file "lhc"

         first_fixpoint <- transformer (replaceExtension file "grin")
                           [ Step "Optimize" Simple.optimize
                           , Step "Remove dead code" DeadCode.trimDeadCode
                           , Step "Inline" Inline.inlinePass ]
                           grin
         let applyLowered = Apply.lower first_fixpoint
             hptEnv = HPT.mkEnvironment applyLowered
             (iterations, hpt) = HPT.analyze applyLowered
             (evalLowered, hpt') = HPT.lower hpt applyLowered
         timeIt "Lowering apply primitives" $ outputGrin target "_apply" applyLowered
         timeIt "Heap points-to analysis" $ do forM_ iterations $ \_ -> do putStr "."; hFlush stdout
                                               outputGrin target "_eval" evalLowered
         putStrLn $ "HPT fixpoint found in " ++ show (length iterations) ++ " iterations."

         let stage2_raw = Stage2.convert hpt' evalLowered
         second_fixpoint <- transformer (replaceExtension file "grin2")
                            [ Step "Optimize" Stage2.Simple.optimize
                            , Step "Remove dead code" Stage2.trimDeadCode
                            , Step "Case optimize" Stage2.Case.optimize
                            , Step "Rename" Stage2.rename
                            , Step "Apply rewrite rules" Stage2.Case.applyRewriteRules
                            , Step "Inline" (Stage2.trimDeadCode . Stage2.Case.inlinePass)
                            , Step "Apply rewrite rules" Stage2.Case.applyRewriteRules
                            , Step "Optimize" (Stage2.Simple.optimize . Stage2.trimDeadCode)
                            ]
                            stage2_raw
         let stage2_out = second_fixpoint
         outputGrin2 target "" stage2_out
         
         case action of
           Benchmark -> timeIt "Compiling C code" $ Backend.C.compileFastCode stage2_out (dropExtension target)
           LLVM      -> timeIt "Compiling LLVM code" $ Backend.LLVM.compile stage2_out target
           Compile   -> timeIt "Compiling C code" $ Backend.C.compile stage2_out (dropExtension target)

outputGrin file variant grin
    = do let outputFile = replaceExtension file ("grin"++variant)
         writeFile outputFile (show $ ppGrin grin)
         return ()

outputGrin2 file variant grin
    = do let outputFile = replaceExtension file ("grin2"++variant)
         writeFile outputFile (show $ Stage2.ppGrin grin)
         return ()

loadAllLibraries :: IO (Map.Map ModuleIdent SimpleModule)
loadAllLibraries
    = do dataDir <- getAppUserDataDirectory "lhc"
         let packageDir = dataDir </> "packages"
         packages <- getDirectoryContents packageDir
         smods <- forM (filter (`notElem` [".",".."]) packages) $ \package ->
                  do modules <- getDirectoryContents (packageDir </> package)
                     forM (filter (`notElem` [".",".."]) modules) $ \mod ->
                       do -- putStrLn $ "Loading: " ++ (packageDir </> package </> mod)
                          smod <- decodeFile (packageDir </> package </> mod)
                          return ((package,mod),smod)
         return $ Map.fromList [ (ident, mod) | (ident,mod) <- concat smods ]


parseCore :: FilePath -> IO SimpleModule
parseCore path
    = do inp <- L.readFile path
         case Core.parseModule path inp of
           Left errs -> error (show errs)
           Right mod -> do --putStrLn $ "parsing done: " ++ path
                           return (coreToSimpleCore mod)

{-
timeIt :: String -> IO a -> IO a
timeIt msg action
    = do printf "%-40s" (msg ++ ": ")
         hFlush stdout
         s <- getCurrentTime
         a <- action
         e <- getCurrentTime
         printf "%.2fs\n" (realToFrac (diffUTCTime e s) :: Double)
         return a
-}