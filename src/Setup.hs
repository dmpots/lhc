module Setup where


import System.Exit
import System.Environment
import System.Console.GetOpt
import System.Directory
import System.IO
import Data.Maybe
import Data.Char

data Action
    = LibCheck
    | Install
    | Build


data ConfigFlag
    = Help

parseArgs :: [String] -> IO (Action)
parseArgs args
    = case getOpt RequireOrder [] args of
        (flags, (cmdStr:args), []) | Just cmd <- lookup cmdStr commands
          -> do 
                --exitWith ExitSuccess
                return cmd
        (flags, _, [])  -> do hPutStrLn stderr "Usage!"
                              exitWith ExitSuccess
        (flags, _, errs) -> do hPutStrLn stderr $ unlines errs
                               exitFailure

commands = [ ("install", Install)
           , ("libcheck", LibCheck) ]

