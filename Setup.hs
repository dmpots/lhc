import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import Control.Monad

main :: IO ()
main = defaultMainWithHooks
         simpleUserHooks{ hookedPreProcessors = hookedPreProcessors simpleUserHooks ++ myPreProcessors }

myPreProcessors = [("hs", \_ _ -> pp)]
    where pp = PreProcessor
                 { platformIndependent = True
                 , runPreProcessor = \(inDir,inFile) (outDir,outFile) verbose
                     -> if normalise (inDir </> inFile) `elem` (map ("src" </>) driftFiles)
                        then do mbDrIFT <- findExecutable "DrIFT"
                                case mbDrIFT of
                                  Just drift -> do putStrLn $ "Processing: " ++ inDir </> inFile
                                                   rawSystemExit verbose drift [inDir </> inFile, "-o", outDir </> outFile]
                                                   let bootFile = inDir </> inFile ++ "-boot"
                                                   hasBoot <- doesFileExist bootFile
                                                   when hasBoot $ copyFile bootFile (outDir </> outFile ++ "-boot")
                                  Nothing    -> do hPutStrLn stderr $ "Could not find DrIFT executable. Exiting."
                                                   exitWith (ExitFailure 1)
                        else do --putStrLn $ "Ignoring: " ++ inDir </> inFile
                                return ()
                 }


driftFiles = words
 "C/FFI.hs C/FromGrin2.hs Cmm/Op.hs C/Prims.hs DataConstructors.hs \
 \DerivingDrift/StandardRules.hs E/CPR.hs E/Demand.hs E/LambdaLift.hs \
 \E/SSimplify.hs E/ToHs.hs E/TypeCheck.hs E/Type.hs FrontEnd/Class.hs \
 \FrontEnd/Exports.hs FrontEnd/HsSyn.hs FrontEnd/KindInfer.hs \
 \FrontEnd/Representation.hs FrontEnd/SrcLoc.hs FrontEnd/Tc/Kind.hs \
 \FrontEnd/Tc/Monad.hs Grin/SSimplify.hs Name/VConsts.hs Options.hs \
 \DataConstructors.hs-boot Ho/Type.hs  Ho/Build.hs"
