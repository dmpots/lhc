module Manager where

import Text.PrettyPrint.ANSI.Leijen
import Text.Printf
import System.FilePath
import System.IO
import Data.Time

data Step a = Step String (a -> a)

type Transformer a = a -> IO a

transformer :: (Eq a, Pretty a) => FilePath -> [Step a] -> Transformer a
transformer target [] firstValue = return firstValue
transformer target steps firstValue
    = worker 0 steps firstValue firstValue
    where worker n [] startValue endValue
              | startValue == endValue
              = do printf "\nFound fixpoint in %d iterations.\n" (n `div` length steps ::Int)
                   return endValue
              | otherwise
              = worker n steps endValue endValue
          worker n (Step name fn:xs) startValue intermediaryValue
              = do let --nStr       = printf "%03d" n            :: String
                       --targetFile = printf "%s_%s" target nStr :: FilePath
                       value = fn intermediaryValue
                   --timeIt (name++"["++nStr++"]") $ writeFile targetFile (show $ pretty value)
                   putStr "." >> hFlush stdout
                   worker (n+1) xs startValue value
                   


timeIt :: String -> IO a -> IO a
timeIt msg action
    = do printf "%-40s" (msg ++ ": ")
         hFlush stdout
         s <- getCurrentTime
         a <- action
         e <- getCurrentTime
         printf "%.2fs\n" (realToFrac (diffUTCTime e s) :: Double)
         return a

{-

let first_loop = transformers "grin" [ step "Optimize" Simple.optimize
                         , step "Remove dead code" DeadCode.trimDeadCode
                         , step "Inline" Inline.inlinePass ]
    
first_fixpoint <- run step1 grin_from_core
let lowered = evalLowered first_fixpoint
    stage2_initial = stage1_to_stage2 first_fixpoint
    second_loop = transformers "grin2" [ step "Optimize" Stage2.Simple.optimize
                                       , step "Remove dead code" trimDeadCode
                                       , step "Rename" rename
                                       , step "Rewrite" rewrite
                                       , step "Inline" inline ]
second_fixpoint <- run second_loop stage2_initial

-}
