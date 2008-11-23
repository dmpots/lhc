-- | Defines some basic version strings about the compiler.
module Version.Version(
  versionSimple -- :: String
, versionString -- :: String
) where
import Data.Version
import System.Info
import Version.Config

-- | Simple version string
{-# NOINLINE versionSimple #-}
versionSimple = concat [package, " ", version, " (", tag, ")"]

-- | Full version string containing OS/compiler info
{-# NOINLINE versionString #-}
versionString = concat [versionSimple, " compiled by ",compilerName,"-",showVersion compilerVersion," on a ",arch," running ",os]
