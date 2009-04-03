module Main where

import System.Directory
import System.FilePath
import System.Environment
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO
import qualified Data.Set as Set
import Data.Binary

import qualified Language.Core as Core
import Grin.SimpleCore
import Grin.FromCore
import Grin.Pretty
import Grin.DeadCode

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> error "No arguments!"
            ["libcheck"]      -> error "TODO"
            ("install":files) -> mapM_ installCoreFile files
            ("build":files)   -> build files


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

build :: [FilePath] -> IO ()
build files
    = do hPutStrLn stderr "Parsing core files..."
         smods <- mapM parseCore files
         hPutStrLn stderr "Tracking core dependencies..."
         allSmods <- loadDependencies smods
         let tdefs = concatMap moduleTypes allSmods
             defs = concatMap moduleDefs allSmods
         let grin = coreToGrin tdefs defs
             reduced = removeDeadCode ["main::Main.main"] grin
         hPutStrLn stderr "Translating to grin..."
         evaluate grin
         hPutStrLn stderr "Removing dead code..."
         evaluate reduced
         hPutStrLn stderr "Printing grin..."
         print (ppGrin reduced)
         return ()

loadDependencies :: [SimpleModule] -> IO [SimpleModule]
loadDependencies smods
    = do let deps = Set.fromList $ concatMap moduleDeps smods
             have = Set.fromList $ ("ghczmprim","GHCziPrim"):[ (modulePackage mod, moduleName mod) | mod <- smods ]
             new  = Set.toList (deps `Set.difference` have)
         --putStrLn $ "New dependencies: " ++ show new
         if null new
            then return smods
            else do newMods <- mapM (uncurry loadLibraryModule) new
                    loadDependencies (smods ++ newMods)

loadLibraryModule :: String -> String -> IO SimpleModule
loadLibraryModule packageName moduleName
    = do dataDir <- getAppUserDataDirectory "lhc"
         let file = dataDir </> packageName </> moduleName
         decodeFile file


parseCore :: FilePath -> IO SimpleModule
parseCore path
    = do inp <- L.readFile path
         case Core.parseModule path inp of
           Left errs -> error (show errs)
           Right mod -> do --putStrLn $ "parsing done: " ++ path
                           return (coreToSimpleCore mod)
