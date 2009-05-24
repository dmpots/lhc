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
import qualified Grin.HtmlAnnotate as Html

--import Grin.Rename
import qualified Grin.HPT as HPT
import qualified Grin.Lowering.Apply as Apply

-- TODO: We need proper command line parsing.
tryMain :: IO ()
tryMain = do args <- getArgs
             case args of
               ("install":files)      -> mapM_ installCoreFile files >> exitWith ExitSuccess
               ("build":file:args)    -> build Build file args >> exitWith ExitSuccess
               ("eval":file:args)     -> build Eval file args >> exitWith ExitSuccess
               ("compile":file:args)  -> build Compile file args >> exitWith ExitSuccess
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

data Action = Build | Eval | Compile

build :: Action -> FilePath -> [String] -> IO ()
build action file args
    = do mod <- parseCore file
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
                          Map.insert (modulePackage mod, moduleName mod) mod libs
             (tdefs, defs) = Simple.removeDeadCode [("main","Main")]  ["main::Main.main"] allModules
             grin = coreToGrin tdefs defs
             opt = iterate Simple.optimize grin !! 2
             applyLowered = Apply.lower opt
             (iterations, hpt) = HPT.analyze applyLowered
             (evalLowered, hpt') = HPT.lower hpt applyLowered
             opt' = iterate Simple.optimize evalLowered !! 2
             out = opt'
         case action of
           Build -> print (ppGrin out)
           Eval  -> Compile.runGrin out "main::Main.main" args >> return ()
           Compile -> do let target = replaceExtension file "lhc"
                         outputGrin target "_raw" grin
                         outputGrin target "_simple" opt
                         outputGrin target "_apply" applyLowered
                         outputGrin target "_eval" evalLowered
                         --outputAnnotation target "_eval.html" Map.empty evalLowered
                         outputGrin target "" out
                         --outputAnnotation target ".html" Map.empty out

                         putStrLn $ "Fixpoint found in " ++ show iterations ++ " iterations."

                         lhc <- findExecutable "lhc"
                         L.writeFile target $ L.unlines [ L.pack $ "#!" ++ fromMaybe "/usr/bin/env lhc" lhc ++ " execute"
                                                        , encode out ]
                         perm <- getPermissions target
                         setPermissions target perm{executable = True}

outputGrin file variant grin
    = do let outputFile = replaceExtension file ("grin"++variant)
         writeFile outputFile (show $ ppGrin grin)
         return ()

outputAnnotation file variant annotation grin
    = do let outputFile = replaceExtension file ("grin"++variant)
         writeFile outputFile (Html.annotate annotation grin)

execute :: FilePath -> [String] -> IO ()
execute path args
    = do inp <- L.readFile path
         let grin = decode (dropHashes inp)
         --eval grin "main::Main.main" args
         Compile.runGrin grin "main::Main.main" args
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
