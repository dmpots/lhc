import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import System.Directory
import System.FilePath
import Control.Monad

main :: IO ()
main = defaultMainWithHooks
         simpleUserHooks{ hookedPreProcessors = hookedPreProcessors simpleUserHooks ++ myPreProcessors }

myPreProcessors = [("hs", \_ _ -> pp)]
    where pp = PreProcessor
                 { platformIndependent = True
                 , runPreProcessor = \(inDir,inFile) (outDir,outFile) verbose
                     -> if normalise ("drift_processed" </> inDir </> inFile) `elem` driftFiles
                        then do --putStrLn $ "Processing: " ++ inDir </> inFile
                                rawSystemExit verbose "DrIFT" [inDir </> inFile, "-o", outDir </> outFile]
                                let bootFile = inDir </> inFile ++ "-boot"
                                hasBoot <- doesFileExist bootFile
                                when hasBoot $ copyFile bootFile (outDir </> outFile ++ "-boot")
                        else do --putStrLn $ "Ignoring: " ++ inDir </> inFile
                                return ()
                 }


driftFiles = words
 "drift_processed/C/FFI.hs drift_processed/C/FromGrin2.hs drift_processed/Cmm/Op.hs drift_processed/C/Prims.hs drift_processed/DataConstructors.hs \
 \drift_processed/DerivingDrift/StandardRules.hs drift_processed/E/CPR.hs drift_processed/E/Demand.hs drift_processed/E/LambdaLift.hs \
 \drift_processed/E/SSimplify.hs drift_processed/E/ToHs.hs drift_processed/E/TypeCheck.hs drift_processed/E/Type.hs drift_processed/FrontEnd/Class.hs \
 \drift_processed/FrontEnd/Exports.hs drift_processed/FrontEnd/HsSyn.hs drift_processed/FrontEnd/KindInfer.hs \
 \drift_processed/FrontEnd/Representation.hs drift_processed/FrontEnd/SrcLoc.hs drift_processed/FrontEnd/Tc/Kind.hs \
 \drift_processed/FrontEnd/Tc/Monad.hs drift_processed/Grin/SSimplify.hs drift_processed/Name/VConsts.hs drift_processed/Options.hs \
 \drift_processed/DataConstructors.hs-boot drift_processed/Ho/Type.hs  drift_processed/Ho/Build.hs"
