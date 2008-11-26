-- | Defines some info about the compiler
module LHCVersion where
import Data.Version
import System.Info

package        = "lhc"
shortVersion   = "0.6"
revision       = "20081123"
version        = concat [shortVersion,".",revision]
tag            = "ryu"

-- | Simple version string
{-# NOINLINE versionSimple #-}
versionSimple = concat [package, " ", version, " (", tag, ")"]

-- | Full version string containing OS/compiler info
{-# NOINLINE versionString #-}
versionString = concat [versionSimple, " compiled by ",compilerName,"-",showVersion compilerVersion," on a ",arch," running ",os]
